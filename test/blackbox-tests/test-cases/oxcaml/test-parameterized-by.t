Test that parametrizing a library work.

  $ . ./helpers.sh
  $ init_project

Creates two parameters.

  $ make_dir_with_dune "foo_intf" <<EOF
  > (library_parameter
  >  (name foo_intf))
  > EOF
  > cat > foo_intf/foo_intf.mli <<EOF
  > type t
  > val foo: t -> unit
  > EOF

  $ make_dir_with_dune "bar_intf" <<EOF
  > (library_parameter
  >  (name bar_intf))
  > EOF
  > cat > bar_intf/foo_intf.mli <<EOF
  > type t
  > val bar: t -> unit
  > EOF

Creates a library parametrized by the foo parameter.

  $ make_lib_parameterized "param_func" "foo_intf"
  $ cat > "param_func/param_func.ml" <<EOF
  > type t = Foo_intf.t
  > let bar (x : t) = Foo_intf.foo x
  > EOF

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null

Creates a library parametrized by two parameters.

  $ rm -rf  _build param_func
  $ make_lib_parameterized "param_func" "foo_intf bar_intf"
  $ cat > "param_func/param_func.ml" <<EOF
  > type foo = Foo_intf.t
  > let foo_f x = Foo_intf.foo x
  > type bar = Bar_intf.t
  > let bar_f x = Bar_intf.bar x
  > EOF

TODO(@maiste): remove /dev/null
FIXME: should be able to have multiple parameters
  $ dune build > /dev/null
  File "param_func/param_func.ml", line 3, characters 11-21:
  3 | type bar = Bar_intf.t
                 ^^^^^^^^^^
  Error: The module Bar_intf
         is a parameter but is not declared as such for the current unit.
  Hint: Compile the current unit with -parameter Bar_intf.
  [1]

Creates a library with a reference to a non-existing type in the parameter.

  $ rm -rf _build param_func
  $ make_lib_parameterized "param_func" "foo_intf"
  $ cat > "param_func/param_func.ml" <<EOF
  > type t = Foo_intf.foo
  > EOF

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null
  File "param_func/param_func.ml", line 1, characters 9-21:
  1 | type t = Foo_intf.foo
               ^^^^^^^^^^^^
  Error: Unbound type constructor Foo_intf.foo
  [1]

Fails when creating a library dependending on a parametrized library but
without instanciation.

  $ rm -rf _build
  $ cat > "param_func/param_func.ml" <<EOF
  > type t = Foo_intf.t
  > let f (_x : t) = ()
  > EOF

  $ make_dir_with_dune "lib" <<EOF
  > (library
  >   (name lib)
  >   (libraries param_func))
  > EOF
  > cat > "lib/lib.ml" <<EOF
  > type t = Param_func.t
  > EOF


TODO(@maiste): remove /dev/null
FIXME: clearer error message and it should be detected earlier
  $ dune build > /dev/null
  File "lib/lib.ml", line 1, characters 9-21:
  1 | type t = Param_func.t
               ^^^^^^^^^^^^
  Error: The module Param_func is not accessible because it takes Foo_intf
         as a parameter and the current unit does not.
  Hint: Pass -parameter Foo_intf to add Foo_intf as a parameter
        of the current unit.
  [1]


It fails when creating an executable using a parametrized library without
instanciation.

  $ rm -rf _build lib

  $ make_dir_with_dune "bin" <<EOF
  > (executable
  >   (name bin)
  >   (libraries param_func))
  > EOF
  > cat > "bin/bin.ml" <<EOF
  > type t = Param_func.t
  > EOF


TODO(@maiste): remove /dev/null
FIXME: clearer error message and it should be detected earlier
  $ dune build > /dev/null
  File "bin/bin.ml", line 1, characters 9-21:
  1 | type t = Param_func.t
               ^^^^^^^^^^^^
  Error: The module Param_func is not accessible because it takes Foo_intf
         as a parameter and the current unit does not.
  Hint: Pass -parameter Foo_intf to add Foo_intf as a parameter
        of the current unit.
  [1]

Create a parametrized library that depends on a parametrized library using the
same parameter.

  $ rm -rf _build bin
  $ make_dir_with_dune "param_func_2" <<EOF
  >  (library
  >    (name param_func_2)
  >    (parameters foo_intf)
  >    (libraries param_func))
  > EOF
  $ cat > "param_func_2/param_func_2.ml" <<EOF
  > let f (x : Foo_intf.t) = Param_func.f x
  > EOF

TODO(@maiste): remove /dev/null
  $ dune build > /dev/null

Create a parametrized library using another parametrized library that has a
subset of its parameters.

  $ rm -rf _build param_func_2
  $ make_dir_with_dune "param_func_2" <<EOF
  >  (library
  >    (name param_func_2)
  >    (parameters foo_intf bar_intf)
  >    (libraries param_func))
  > EOF
  $ cat > "param_func_2/param_func_2.ml" <<EOF
  > type bar = Bar_intf.t
  > let f (x : Foo_intf.t) = Param_func.f x
  > EOF

TODO(@maiste): remove /dev/null
FIXME: use multiple parameters
  $ dune build > /dev/null
  File "param_func_2/param_func_2.ml", line 1, characters 11-21:
  1 | type bar = Bar_intf.t
                 ^^^^^^^^^^
  Error: The module Bar_intf
         is a parameter but is not declared as such for the current unit.
  Hint: Compile the current unit with -parameter Bar_intf.
  [1]

It fails to build a parametrized library using another library that has a
superset of its parameter. It must always be a subset.


  $ rm -rf _build param_func param_func_2
  $ make_lib_parameterized "param_func" "foo_intf bar_intf"
  $ cat > "param_func/param_func.ml" <<EOF
  > type foo = Foo_intf.t
  > let foo_f x = Foo_intf.foo x
  > type bar = Bar_intf.t
  > let bar_f x = Bar_intf.bar x
  > EOF
  $ make_dir_with_dune "param_func_2" <<EOF
  >  (library
  >    (name param_func_2)
  >    (parameters foo_intf)
  >    (libraries param_func))
  > EOF
  $ cat > "param_func_2/param_func_2.ml" <<EOF
  > let f (x : Foo_intf.t) = Param_func.f x
  > EOF

TODO(@maiste): remove /dev/null
FIXME: use multiple parameters and should fail
  $ dune build > /dev/null
  File "param_func/param_func.ml", line 3, characters 11-21:
  3 | type bar = Bar_intf.t
                 ^^^^^^^^^^
  Error: The module Bar_intf
         is a parameter but is not declared as such for the current unit.
  Hint: Compile the current unit with -parameter Bar_intf.
  [1]
