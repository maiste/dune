Test that opening files in parameterized library work and that type equality
from implementation is exposed.

  $ . ./helpers.sh
  $ init_project

  $ make_dir_with_dune "param_intf" <<EOF
  > (library_parameter
  >  (name param_intf))
  > EOF
  $ make_dummy_intf "param_intf" "param_intf"

  $ make_lib_impl "param_impl" "param_intf"
  $ make_lib_parameterized "param_func" "param_intf"
  $ cat > "param_func/param_func_import.ml" <<EOF
  > module P = Param_intf
  > EOF
  $ cat > "param_func/param_func.ml" <<EOF
  > open Param_func_import
  > let f x = P.f x
  > EOF

  $ make_dir_with_dune "bin" <<EOF
  > (executable
  >  (name main)
  >  (libraries ((param_func param_impl) :as inst)))
  > EOF
  $ cat > "bin/main.ml" <<EOF
  > let () = Inst.f 2
  > EOF

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null
  File "bin/dune", line 3, characters 12-46:
  3 |  (libraries ((param_func param_impl) :as inst)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]
