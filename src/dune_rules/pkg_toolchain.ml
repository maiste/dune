open Import

let base_dir () =
  let cache_dir =
    Lazy.force Dune_util.xdg |> Xdg.cache_dir |> Path.Outside_build_dir.of_string
  in
  let path =
    Path.Outside_build_dir.relative
      (Path.Outside_build_dir.relative cache_dir "dune")
      "toolchains"
  in
  (let path = Path.outside_build_dir path in
   if not (Path.Untracked.exists path) then Path.mkdir_p path;
   if not (Path.Untracked.is_directory path)
   then
     User_error.raise
       [ Pp.textf "Expected %s to be a directory but it is not." (Path.to_string path) ]);
  path
;;

let make_bool ~name ~default =
  let of_string s =
    match Bool.of_string s with
    | Some b -> Ok b
    | None ->
      Error (sprintf "%s is not a bool (must be \"true\" or \"false\")" (String.quoted s))
  in
  Config.make ~name ~of_string ~default
;;

let enabled = make_bool ~name:"toolchains_enabled" ~default:false

let pkg_dir (pkg : Dune_pkg.Lock_dir.Pkg.t) =
  (* The name of this package's directory within the toolchains
     directory. Includes a hash of some of the package's fields so that
     if a user modifies a package's lockfile in one project, then the
     modified package won't be used in other projects (unless the
     corresponding lockfile in those projects is modified in the same
     way). *)
  let dir_name =
    (* TODO should include resolved deps *)
    let pkg_hash = Digest.generic pkg in
    (* A hash of the fields of a package that affect its installed artifacts *)
    sprintf
      "%s.%s-%s"
      (Package.Name.to_string pkg.info.name)
      (Package_version.to_string pkg.info.version)
      (Digest.to_string pkg_hash)
  in
  Path.Outside_build_dir.relative (base_dir ()) dir_name
;;

let installation_prefix ~pkg_dir = Path.Outside_build_dir.relative pkg_dir "target"

let is_compiler_and_toolchains_enabled name =
  Config.get enabled
  &&
  let module Package_name = Dune_pkg.Package_name in
  let compiler_package_names =
    (* TODO don't hardcode these names here *)
    [ Package_name.of_string "ocaml-base-compiler"
    ; Package_name.of_string "ocaml-variants"
    ]
  in
  List.mem compiler_package_names name ~equal:Package_name.equal
;;

let files ~bin_dir =
  let open Memo.O in
  Fs_memo.dir_contents bin_dir
  >>| function
  | Error _ -> Section.Map.empty
  | Ok files ->
    let bin_paths =
      Fs_cache.Dir_contents.to_list files
      |> List.filter_map ~f:(fun (filename, kind) ->
        match kind with
        | Unix.S_REG | S_LNK ->
          let path = Path.Outside_build_dir.relative bin_dir filename in
          (try
             Unix.access (Path.Outside_build_dir.to_string path) [ Unix.X_OK ];
             Some (Path.outside_build_dir path)
           with
           | Unix.Unix_error _ -> None)
        | _ -> None)
    in
    Section.Map.singleton Section.Bin bin_paths
;;

let ocaml context env ~bin_dir =
  let which prog =
    let open Memo.O in
    let path = Path.Outside_build_dir.relative bin_dir prog in
    let+ exists = Fs_memo.file_exists path in
    if exists then Some (Path.outside_build_dir path) else None
  in
  let get_ocaml_tool ~dir:_ prog = which prog in
  Ocaml_toolchain.make context ~which ~env ~get_ocaml_tool
;;

(* The path to the directory containing the artifacts within the
   temporary install directory. When installing with the DESTDIR
   variable, the absolute path to the final installation directory is
   concatenated to the value of DESTDIR. *)
let installation_prefix_within_tmp_install_dir ~installation_prefix:prefix tmp_install_dir
  =
  let target_without_root_prefix =
    (* Remove the root directory prefix from the target directory so
       it can be used to create a path relative to the temporary
       install dir. *)
    match
      String.drop_prefix
        (Path.Outside_build_dir.to_string prefix)
        ~prefix:(Path.External.to_string Path.External.root)
    with
    | Some x -> x
    | None ->
      Code_error.raise
        "Expected prefix to start with root"
        [ "prefix", Path.Outside_build_dir.to_dyn prefix
        ; "root", Path.External.to_dyn Path.External.root
        ]
  in
  Path.relative tmp_install_dir target_without_root_prefix
;;

let build_action =
  Dune_lang.Action.Run
    [ Slang.text "touch"
    ; Slang.concat
        [ Slang.pform (Pform.Var (Pform.Var.Pkg Pform.Var.Pkg.Build))
        ; Slang.text "/config.cache"
        ]
    ]
;;

let modify_install_action (action : Dune_lang.Action.t) ~installation_prefix ~suffix =
  match action with
  | Run [ Literal make; Literal install ] ->
    (match String_with_vars.pform_only make, String_with_vars.text_only install with
     | Some (Pform.Var Pform.Var.Make), Some "install" ->
       let tmp_install_dir = Temp.create Dir ~prefix:"dune-toolchain-destdir" ~suffix in
       let action =
         (* Set the DESTDIR variable so installed artifacts are not immediately
            placed in the final installation directory. *)
         Dune_lang.Action.Run
           [ Literal make
           ; Literal install
           ; Slang.text (sprintf "DESTDIR=%s" (Path.to_string tmp_install_dir))
           ]
       in
       let prefix = Path.outside_build_dir installation_prefix in
       (* Append some commands to the install command that copy
          the artifacts to their final installation directory. *)
       Dune_lang.Action.Progn
         [ action
         ; Run
             [ Slang.text "mkdir"
             ; Slang.text "-p"
             ; Slang.text @@ Path.to_string @@ Path.parent_exn prefix
             ]
         ; Run
             [ Slang.text "mv"
             ; (* Prevents mv from replacing the destination if it
                  already exists. This can happen if two dune
                  instances race to install the toolchain. Note
                  that -n is not posix but it is supported by gnu
                  coreutils and by the default mv command on
                  macos, but not openbsd. *)
               Slang.text "-n"
             ; Slang.text
                 (Path.to_string
                  @@ installation_prefix_within_tmp_install_dir
                       ~installation_prefix
                       tmp_install_dir)
             ; Slang.text @@ Path.to_string @@ Path.parent_exn prefix
             ]
         ]
     | _ ->
       (* The install command is something other than `make install`, so don't
          attempt to modify. *)
       action)
  | _ ->
    (* Not a "run" action, so don't attempt to modify. *)
    action
;;

let modify_install_action ~prefix ~suffix action =
  let open Memo.O in
  let+ installed = Fs_memo.dir_exists prefix in
  if installed
  then
    (* Replace install command with no-op if the toolchain is already installed. *)
    Dune_lang.Action.Progn []
  else modify_install_action action ~installation_prefix:prefix ~suffix
;;

module Override_pform = struct
  type t =
    { prefix : Path.t
    ; doc : Path.t
    }

  let make ~installation_prefix =
    let prefix = Path.outside_build_dir installation_prefix in
    { prefix; doc = Path.relative prefix "doc" }
  ;;
end
