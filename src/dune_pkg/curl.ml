open Import
open Fiber.O

let bin =
  lazy
    (match Bin.which ~path:(Env_path.path Env.initial) "curl" with
     | Some p -> p
     | None ->
       User_error.raise
         ~hints:[ Pp.text "Install curl with your system package manager." ]
         [ Pp.text
             "The program \"curl\" does not appear to be installed. Dune uses curl to \
              download packages. Dune requires that the \"curl\" executable be located \
              in one of the directories listed in the PATH variable."
         ])
;;

let user_agent =
  lazy
    (let base = "dune" in
     match Build_info.V1.version () with
     | None -> base
     | Some v -> base ^ "." ^ Build_info.V1.Version.to_string v)
;;

let curl_features_regex =
  (* If these features are present, then --compressed is supported *)
  let features = [ "libz"; "brotli"; "zstd" ] in
  Re.compile
    Re.(seq [ bol; str "Features:"; rep (first (alt (List.map ~f:str features))) ])
;;

let compressed_supported =
  (* We check if curl supports --compressed by running curl -V and checking if
     the output contains the features we need. *)
  Fiber_lazy.create (fun () ->
    let+ lines, _ =
      let stderr_to =
        Process.Io.make_stderr
          ~output_on_success:Swallow
          ~output_limit:Dune_engine.Execution_parameters.Action_output_limit.default
      in
      Process.run_capture_lines Return ~stderr_to ~display:Quiet (Lazy.force bin) [ "-V" ]
    in
    match List.find_map lines ~f:(Re.exec_opt curl_features_regex) with
    | Some group -> Re.Group.test group 0
    | None -> false)
;;

let run ~url ~temp_dir ~output =
  let* compressed_supported = Fiber_lazy.force compressed_supported in
  let args =
    List.flatten
      [ [ "-L"
        ; "-s"
        ; "--user-agent"
        ; Lazy.force user_agent
        ; "--write-out"
        ; "\"%{http_code}\"" (* This arg must be quoted to work on windows *)
        ; "-o"
        ; Path.to_string output
        ]
      ; (if compressed_supported then [ "--compressed" ] else [])
      ; [ "--"; url ]
      ]
  in
  let stderr = Path.relative temp_dir "curl.stderr" in
  let+ http_code, exit_code =
    let stderr_to = Process.Io.file stderr Out in
    Process.run_capture_line
      Return
      ~stderr_to
      ~display:!Dune_engine.Clflags.display
      (Lazy.force bin)
      args
  in
  if exit_code <> 0
  then (
    let stderr =
      match Io.read_file stderr with
      | s ->
        Path.unlink_no_err stderr;
        [ Pp.text s ]
      | exception s ->
        [ Pp.textf
            "failed to read stderr form file %s"
            (Path.to_string_maybe_quoted stderr)
        ; Exn.pp s
        ]
    in
    Error
      (User_message.make
         ([ Pp.textf "curl returned an invalid error code %d" exit_code ] @ stderr)))
  else (
    Path.unlink_no_err stderr;
    match
      let open Option.O in
      let suffix = {|"|} in
      let prefix = suffix in
      String.drop_prefix_and_suffix ~prefix ~suffix http_code >>= Int.of_string
    with
    | None ->
      Error
        (User_message.make
           [ Pp.textf "curl returned an HTTP code we don't understand: %S" http_code ])
    | Some http_code ->
      if http_code = 200
      then Ok ()
      else Error (User_message.make [ Pp.textf "download failed with code %d" http_code ]))
;;
