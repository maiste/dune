Test that the implementation works as expected.

  $ . ./helpers.sh

Two simple parameters.

  $ init_project

  $ create_dune_dir "foo" <<EOF
  > (library_parameter
  >   (name foo))
  > EOF

  $ cat > foo/foo.mli <<EOF
  > type t
  > val foo: t -> unit
  > EOF

  $ create_dune_dir "bar" <<EOF
  > (library_parameter
  >   (name bar))
  > EOF

  $ cat > bar/bar.mli <<EOF
  > type t
  > val bar: t -> unit
  > EOF

TODO(@maiste) improve the handling of the result
  $ dune build > /dev/null

A library implementing the parameter.

  $ clean_build

  $ create_dune_dir "foo_impl" <<EOF
  > (library
  >  (name foo_impl)
  >  (implements foo))
  > EOF

  $ cat > foo_impl/foo_impl.ml <<EOF
  > type t = int
  > let foo _ = ()
  > let ignore_me = 42
  > EOF

A library implementing the parameter with a bigger interface than
what the parameter expects.

  $ clean_build
  $ rm -rf foo_impl

  $ create_dune_dir "foo_impl" <<EOF
  > (library
  >  (name foo_impl)
  >  (implements foo))
  > EOF

  $ cat > foo_impl/foo_impl.ml <<EOF
  > type t = int
  > let foo _ = ()
  > let ignore_me = 42
  > EOF

TODO(@maiste) improve the handling of the result
  $ dune build foo_impl > /dev/null


A library implementing a parameter with the wrong interface.

  $ clean_build
  $ rm -rf foo_impl

  $ create_dune_dir "foo_impl" <<EOF
  > (library
  >  (name foo_impl)
  >  (implements foo))
  > EOF

  $ cat > foo_impl/foo_impl.ml <<EOF
  > type t = int
  > EOF

TODO(@maiste) improve the handling of the result
  $ dune build > /dev/null
  File "foo_impl/foo_impl.ml", line 1:
  Error: The argument module foo_impl/foo_impl.ml
         does not match the parameter signature foo/.foo.objs/byte/foo.cmi: 
         The value foo is required but not provided
         File "foo/foo.mli", line 2, characters 0-18: Expected declaration
  [1]

A library implementing the parameter, but importing the content from other files.

  $ clean_build
  $ rm -rf foo_impl

  $ create_dune_dir "foo_impl" <<EOF
  > (library
  >  (name foo_impl)
  >  (implements foo))
  > EOF

  $ cat > foo_impl/aux_type.ml <<EOF
  > type t = int
  > EOF
  $ cat > foo_impl/aux_type.mli <<EOF
  > type t
  > EOF

  $ cat > foo_impl/aux_impl.ml <<EOF
  > let foo _ = ()
  > EOF
  $ cat > foo_impl/aux_impl.mli <<EOF
  > val foo: Aux_type.t -> unit
  > EOF

  $ cat > foo_impl/foo_impl.ml <<EOF
  > include Aux_type
  > include Aux_impl
  > EOF

TODO(@maiste) improve the handling of the result
  $ dune build > /dev/null
