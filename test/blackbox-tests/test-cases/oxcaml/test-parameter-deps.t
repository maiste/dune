Test that the parameters support access to the dependencies

  $ . ./helpers.sh
  $ init_project

Creates a library that exports a module we can later import as part of the library

  $ make_dir_with_dune "signature" <<EOF
  > (library
  >  (name signature))
  > EOF
  $ cat > "signature/signature.ml" <<EOF
  > module type S = sig
  >  val some_int: int
  > end
  > EOF

Use the library in a library_parameter.

  $ make_dir_with_dune "param_intf" <<EOF
  > (library_parameter
  >  (name param_intf)
  >  (libraries signature))
  > EOF
  $ cat > "param_intf/param_intf.mli" <<EOF
  > module M : Signature.S
  > EOF

Generate an implementation of it and ensure it builds.

  $ make_dir_with_dune "param_impl" <<EOF
  > (library
  >  (name param_impl)
  >  (implements param_intf))
  > EOF
  $ cat > "param_impl/param_impl.ml" <<EOF
  > module M = struct
  >  let some_int = 42
  > end
  > EOF

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null

Create a parameterized library and make sure it build.

  $ make_lib_parameterized "param_func" "param_intf"
  > cat > "param_func/param_func.ml" <<EOF
  > let some_int_from_space = Param_intf.M.some_int
  > EOF

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null

We create an instanciation of the parametrized library with its implementation.

  $ make_dir_with_dune "param_inst" << EOF
  > (executable
  >  (name param_inst)
  >  (libraries (param_func param_impl) :as param))
  > EOF
  > cat > "param_inst/param_inst.ml" <<EOF
  > let _ = print_endline (Int.to_string Param.some_int_from_space)
  > EOF

TODO(@maiste): remove /dev/null
  $ echo "TODO" # dune build ./param_inst/param_inst.exe > /dev/null
  TODO
