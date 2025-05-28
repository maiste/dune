Test rules to generate library parameter.

  $ . ./helpers.sh
  $ init_project

Create a simple library parameter.

  $ make_dir_with_dune "param_intf" <<EOF
  > (library_parameter
  >   (name param_intf))
  > EOF
  $ make_dummy_intf "param_intf" "param_intf"

TODO(@maiste): remove /dev/null
  $ dune build $(target_cmi "param_intf")  > /dev/null

Check that two library paremeters with a colliding name can't be build.

  $ rm -rf _build
  $ make_dir_with_dune "param_intf_2" <<EOF
  > (library_parameter
  >   (name param_intf))
  > EOF
  $ make_dummy_intf "param_intf_2" "param_intf"
  $ make_lib_impl "param_impl" "param_intf"

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null
  File "param_intf/dune", lines 1-2, characters 0-39:
  1 | (library_parameter
  2 |   (name param_intf))
  Error: Library with name "param_intf" is already defined in
  param_intf_2/dune:1. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  [1]

Library parameter is not allowed to have implementations and must only be one
module.

  $ rm -rf  _build param_impl param_intf_2
  $ cat > param_intf/param_intf.ml <<EOF
  > type t = int
  > let f _ = ()
  > EOF

TODO @maiste: improve error display
FIXME: it needs to be an error message about parameter, not about virtual module
  $ dune build $(target_cmi "param") > /dev/null
  File "param_intf/dune", lines 1-2, characters 0-39:
  1 | (library_parameter
  2 |   (name param_intf))
  Error: The following modules have an implementation, they cannot be listed as
  virtual:
  - Param_intf
  [1]

  $ rm -rf _build param_intf/param_intf.ml
  $ cat > param_intf/aux.mli <<EOF
  > type t
  > val create: unit -> t
  > EOF
  $ cat > param_intf/aux.ml <<EOF
  > type t = int
  > let create () = 42
  > EOF

TODO(@maiste): remove /dev/null
FIXME: it needs a specific error message
  $ dune build $(target_cmi "param_intf") > /dev/null
  File "param_intf/dune", lines 1-2, characters 0-39:
  1 | (library_parameter
  2 |   (name param_intf))
  Error: The following modules have an implementation, they cannot be listed as
  virtual:
  - Aux
  [1]

  $ rm -rf param_intf/aux.ml param_intf/aux.mli param_intf/param_intf.mli
  $ make_lib_impl "param_impl" "param_intf"

TODO(@maiste): remove /dev/null
FIXME: it needs a specif error message
  $ dune build > /dev/null 
  File "param_impl/dune", lines 1-3, characters 0-55:
  1 | (library
  2 |   (name param_impl)
  3 |   (implements param_intf))
  Error: Implementations of wrapped libraries cannot introduce new public
  modules.
  The following modules:
  - Param_impl
  must all be marked as private using the (private_modules ..) field.
  [1]


A library paramter must support the modules field.

  $ rm -rf _build
  $ make_dir_with_dune "param_intf" <<EOF
  > (library_parameter
  >  (name param_intf)
  >  (modules param_intf))
  > EOF
  $ make_dummy_intf "param_intf" "param_intf"
 
TODO(@maiste): remove /dev/null
  $ dune build > /dev/null

  $ rm -rf _build
  $ cat >> "param_intf/dune" <<EOF
  > (library
  >   (name lib)
  >   (modules lib))
  > EOF
  $ touch "param_intf/lib.ml"

FIXME: This actually fails but is not supposed to. This is because we don't
support the `modules` field in `library_parameter` stanza.

  $ dune build > /dev/null
  File "param_intf/dune", lines 1-3, characters 0-60:
  1 | (library_parameter
  2 |  (name param_intf)
  3 |  (modules param_intf))
  Error: These modules appear in the virtual_modules field:
  - Lib
  They must also appear in the modules field.
  [1]

Dune should accept multiple library parameters inside the same directory using
modules.

  $ rm -rf _build param_intf param_impl
  $ make_dir_with_dune "parameters" <<EOF
  > (library_parameter
  >  (name param_intf_1)
  >  (modules param_intf_1))
  > (library_parameter
  >  (name param_intf_2)
  >  (modules param_intf_2))
  > (library_parameter
  >  (name param_intf_3)
  >  (modules param_intf_3))
  > EOF
  $ make_dummy_intf "parameters" "param_intf_1"
  $ make_dummy_intf "parameters" "param_intf_2"
  $ make_dummy_intf "parameters" "param_intf_3"
  $ make_lib_impl "param_impl" "param_intf_1"

TODO(@maiste): remove /dev/null
FIXME: this fails because we don't support the module field in the
`library_parameter` stanza.
  $ dune build > /dev/null
  File "parameters/dune", lines 1-3, characters 0-64:
  1 | (library_parameter
  2 |  (name param_intf_1)
  3 |  (modules param_intf_1))
  Error: These modules appear in the virtual_modules field:
  - Param_intf_2
  - Param_intf_3
  They must also appear in the modules field.
  File "parameters/dune", lines 4-6, characters 0-64:
  4 | (library_parameter
  5 |  (name param_intf_2)
  6 |  (modules param_intf_2))
  Error: These modules appear in the virtual_modules field:
  - Param_intf_1
  - Param_intf_3
  They must also appear in the modules field.
  File "parameters/dune", lines 7-9, characters 0-64:
  7 | (library_parameter
  8 |  (name param_intf_3)
  9 |  (modules param_intf_3))
  Error: These modules appear in the virtual_modules field:
  - Param_intf_1
  - Param_intf_2
  They must also appear in the modules field.
  [1]






