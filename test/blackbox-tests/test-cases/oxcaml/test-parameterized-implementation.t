Test the scenario where we implements parameters

  $ . ./helpers.sh
  $ init_project

We create a two library parameters.

  $ make_dir_with_dune "number_intf" <<EOF
  > (library_parameter
  >  (name number_intf))
  > EOF
  $ cat > "number_intf/number_intf.mli" <<EOF
  > type t
  > val one : t
  > val add : t -> t -> t
  > EOF

  $ make_dir_with_dune "add_intf" <<EOF
  > (library_parameter
  >  (name add_intf))
  > EOF
  $ cat > "add_intf/add_intf.mli" <<EOF
  > val add : int -> int -> int
  > EOF

We create an implementation of the addition

  $ make_dir_with_dune "simple_int_impl" <<EOF
  > (library
  >  (name simple_int_impl)
  >  (implements add_intf))
  > EOF
  $ cat > "simple_int_impl/simple_int_impl.ml" <<EOF
  > let add = (+)
  > EOF

We create an implementation of a number parametrized by an addition operation.

  $ make_dir_with_dune "int_impl" <<EOF
  > (library
  >  (name int_impl)
  >  (implements number_intf)
  >  (parameters add_intf))
  > EOF
  $ cat > "int_impl/int_impl.ml" <<EOF
  > type t = int
  > let one = 1
  > let add = Add_intf.add
  > EOF

We create an implementation of the number parameter.

  $ make_dir_with_dune "two" <<EOF
  > (library
  >  (name two)
  >  (parameters number_intf))
  > EOF
  $ cat > "two/two.ml" <<EOF
  > let two = Number_intf.add Number_intf.one Number_intf.one
  > EOF

We create a parametrized library using an implementation of the two combined
with the add interface.

  $ make_dir_with_dune "int_two" <<EOF
  > (library
  >  (name int_two)
  >  (libraries ((two int_impl) :as two))
  >  (parameters add_intf))
  > EOF
  $ cat > "int_two/int_two.ml" <<EOF
  > let two = Two.two
  > EOF

TODO(@maiste): remove /dev/null
FIXME: support instantiation
  $ dune build > /dev/null
  File "int_two/dune", line 3, characters 12-36:
  3 |  (libraries ((two int_impl) :as two))
                  ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

We generate an executable that depends on the implementation of the library.

  $ make_dir_with_dune "bin" <<EOF
  > (executable
  >  (name main)
  >  (libraries (int_two simple_int_impl) :as lib_int)) 
  > EOF
  $ cat > "bin/main.ml" <<EOF
  > let _ = print_endline (string_of_int Lib_int.two)
  > EOF

FIXME: support instantiation
  $ dune exec -- bin.main.exe
  File "bin/dune", line 3, characters 12-37:
  3 |  (libraries (int_two simple_int_impl) :as lib_int)) 
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  File "int_two/dune", line 3, characters 12-36:
  3 |  (libraries ((two int_impl) :as two))
                  ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

