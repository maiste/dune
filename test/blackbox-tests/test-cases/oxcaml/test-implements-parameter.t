Test that the implementation works as expected.

  $ . ./helpers.sh

Two simple parameters.

  $ init_project

  $ make_dir_with_dune "foo" <<EOF
  > (library_parameter
  >   (name foo))
  > EOF
  $ make_dummy_intf "foo" "foo"

  $ make_dir_with_dune "bar" <<EOF
  > (library_parameter
  >   (name bar))
  > EOF
  $ make_dummy_intf "bar" "bar"

TODO(@maiste): remove /dev/null
  $ dune build $(target_cmi "foo")> /dev/null

A library implementing the parameter.

  $ rm -rf _build
  $ make_lib_impl "foo_impl" "foo"

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null

A library implementing the parameter with a bigger interface than
what the parameter expects.

  $ rm -rf _build
  $ echo "let ignore_me = 42" >> foo_impl/foo_impl.ml

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null

A library implementing a parameter with the wrong interface.

  $ rm -rf _build
  $ echo "type t = int" > foo_impl/foo_impl.ml

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null
  File "foo_impl/foo_impl.ml", line 1:
  Error: The argument module foo_impl/foo_impl.ml
         does not match the parameter signature foo/.foo.objs/byte/foo.cmi: 
         The value f is required but not provided
         File "foo/foo.mli", line 2, characters 0-17: Expected declaration
  [1]

A library implementing the parameter, but importing the content from other files.

  $ rm -rf _build

  $ echo "type t = int" > foo_impl/aux_type.ml
  $ echo "type t" > foo_impl/aux_type.mli

  $ echo "let f _ = ()" > foo_impl/aux_impl.ml
  $ echo "val f: Aux_type.t -> unit" > foo_impl/aux_impl.mli

  $ cat > foo_impl/foo_impl.ml <<EOF
  > include Aux_type
  > include Aux_impl
  > EOF

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null
