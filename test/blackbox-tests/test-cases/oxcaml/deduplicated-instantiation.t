
  $ . ./helpers.sh
  $ init_project

  $ make_dir_with_dune "param_intf" <<EOF
  > (library_parameter
  >   (name param_intf))
  > EOF
  $ make_dummy_intf "param_intf" "param_intf"
  $ make_lib_impl "param_impl" "param_intf"


  $ make_dir_with_dune "parameterized" <<EOF
  > (library
  >  (name parameterized)
  >  (parameters param_intf))
  > EOF
  $ cat > "parameterized/parameterized.ml" <<EOF
  > type t = Param_intf.t
  > EOF

  $ make_dir_with_dune "a" <<EOF
  > (library
  >  (name a)
  >  (libraries ((parameterized param_impl) :as param_a)))
  > EOF
  $ cat > "a/a.ml" <<EOF
  > type t = Param_a.t
  > EOF

  $ make_dir_with_dune "b" <<EOF
  > (library
  >  (name b)
  >  (libraries ((parameterized param_impl) :as param_b)))
  > EOF
  $ cat > "b/b.ml" <<EOF
  > type t = Param_b.t
  > EOF

  $ make_dir_with_dune "bin" <<EOF
  > (executable
  >  (name main)
  >  (libraries a b))
  > EOF
  $ cat > "bin/main.ml" <<EOF
  > type t = A.t
  > type t' = B.t
  > EOF

TODO(@maiste): remove /dev/null
FIXME: instantiation
  $ dune build > /dev/null
  File "a/dune", line 3, characters 12-52:
  3 |  (libraries ((parameterized param_impl) :as param_a)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  File "b/dune", line 3, characters 12-52:
  3 |  (libraries ((parameterized param_impl) :as param_b)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]




