Test error messages for invalid instances.

  $ . ./helpers.sh
  $ init_project

  $ make_dir_with_dune "p1_intf" <<EOF
  > (library_parameter
  >  (name p1_intf))
  > EOF
  $ cat > "p1_intf/p1_intf.mli" <<EOF
  > val x : unit -> int
  > EOF

  $ make_dir_with_dune "p2_intf" <<EOF
  > (library_parameter
  >  (name p2_intf))
  > EOF
  $ cat > "p2_intf/p2_intf.mli" <<EOF
  > val x : unit -> int
  > EOF

  $ make_dir_with_dune "p1_impl" <<EOF
  > (library
  >  (name p1_impl)
  >  (implements p1_intf))
  > EOF
  $ cat > "p1_impl/p1_impl.ml" <<EOF
  > let x () = 42
  > EOF

  $ make_dir_with_dune "p1_impl_2" <<EOF
  > (library
  >  (name p1_impl_2)
  >  (implements p1_intf))
  > EOF
  $ cat > "p1_impl_2/p1_impl_2.ml" <<EOF
  > let x () = 42
  > EOF

  $ make_dir_with_dune "p2_impl" <<EOF
  > (library
  >  (name p2_impl)
  >  (implements p2_intf))
  > EOF
  $ cat > "p2_impl/p2_impl.ml" <<EOF
  > let x () = 43
  > EOF

Try to instantiate a library that is not parametrized.

  $ make_dir_with_dune "simple_lib" <<EOF
  > (library
  >  (name simple_lib))
  > EOF
  $ echo "" > simple_lib/simple_lib.ml

  $ make_dir_with_dune "lib" <<EOF
  > (library
  >  (name main)
  >  (libraries ((simple_lib p1_impl) :as foo)))
  > EOF
  $ echo "" > lib/lib.ml

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null
  File "lib/dune", line 3, characters 12-42:
  3 |  (libraries ((simple_lib p1_impl) :as foo)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

Try to instantiate a library with the wrong parameter.

  $ rm -rf _build lib

  $ make_dir_with_dune "parametrized" <<EOF
  > (library
  >  (name parametrized)
  >  (parameters p1_intf))
  > EOF
  $ echo "" > parametrized/parametrized.ml

  $ make_dir_with_dune "lib" <<EOF
  > (library
  >  (name main)
  >  (libraries ((parametrized p2_impl) :as foo)))
  > EOF
  $ echo "" > lib/lib.ml

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null
  File "lib/dune", line 3, characters 12-44:
  3 |  (libraries ((parametrized p2_impl) :as foo)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

Try to instantiate a library with a library that doesn't implement the
parameters. We use the [parametrized] library.

  $ rm -rf _build lib
  $ make_dir_with_dune "lib" <<EOF
  > (library
  >  (name main)
  >  (libraries ((parametrized simple_lib) :as foo)))
  > EOF
  $ echo "" > lib/lib.ml

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null
  File "lib/dune", line 3, characters 12-47:
  3 |  (libraries ((parametrized simple_lib) :as foo)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

Try to instantiate a library with the same parameter twice. We use
[parametrized] to do the check.

  $ rm -rf _build lib
  $ make_dir_with_dune "lib" <<EOF
  > (library
  >  (name main)
  >  (libraries ((parametrized p1_impl p1_impl_2) :as foo)))
  > EOF
  $ echo "" > lib/lib.ml

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null
  File "lib/dune", line 3, characters 12-54:
  3 |  (libraries ((parametrized p1_impl p1_impl_2) :as foo)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

Trying to instantiate with a subset of parameters without specifying missing
parameters. It is not supported.

  $ rm -fr _build lib parametrized 
  $ make_dir_with_dune "parametrized" <<EOF
  > (library
  >  (name parametrized)
  >  (parameters p1_intf p2_intf))
  > EOF
  $ echo "" > parametrized/parametrized.ml

  $ rm -rf _build lib parametrized
  $ make_dir_with_dune "lib" <<EOF
  > (library
  >  (name main)
  >  (libraries ((parametrized p1_impl) :as foo)))
  > EOF
  $ echo "" > lib/lib.ml

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null
  File "lib/dune", line 3, characters 12-44:
  3 |  (libraries ((parametrized p1_impl) :as foo)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]
