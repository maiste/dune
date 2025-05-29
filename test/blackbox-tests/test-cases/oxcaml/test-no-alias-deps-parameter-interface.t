Test interaction between exporting instances, no alias dependencies and parametrized libraries

  $ . ./helpers.sh
  $ init_project

  $ make_dir_with_dune "param_intf" <<EOF
  > (library_parameter
  >  (name param_intf))
  > EOF
  $ cat > "param_intf/param_intf.mli" <<EOF
  > type t
  > EOF

  $ make_dir_with_dune "int_impl" <<EOF
  > (library
  >  (name int_impl)
  >  (implements param_intf))
  > EOF
  > cat > "int_impl/int_impl.ml" <<EOF
  > type t = int
  > EOF

  $ make_dir_with_dune "param_func" <<EOF
  > (library
  >  (name param_func)
  >  (parameters param_intf))
  > EOF
  $ cat > "param_func/param_func.ml" <<EOF
  > module T = struct
  >  type t = Param_intf.t
  > end
  > EOF

  $ make_dir_with_dune "lib" <<EOF
  > (library
  >  (name lib)
  >  (libraries ((param_func int_impl) :as param_int)))
  > EOF
  $ cat > "lib/lib.ml" <<EOF
  > module M = Lib_int
  > tyoe t = Lib_int.T.t
  > EOF

  $ make_dir_with_dune "lib2" <<EOF
  > (library
  >  (name lib2)
  >  (libraries lib))
  > EOF
  $ cat > "lib2/import.ml" <<EOF
  > module M = Lib.M
  > EOF
  $ cat > "lib2/lib2.ml" <<EOF
  > open Import
  > type t = M.T.t
  > EOF

TODO(@maiste): remove /dev/null
FIXME: we should support the instantiation
  $ dune build > /dev/null
  File "lib/dune", line 3, characters 12-49:
  3 |  (libraries ((param_func int_impl) :as param_int)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

