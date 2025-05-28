Test parametrized libary depending on another parametrized library sharing the
same set of parameters.

  $ . ./helpers.sh
  $ init_project

Creates a simple structure with a parameter and its implementation.

  $ make_dir_with_dune "foo_intf" <<EOF
  > (library_parameter
  >  (name foo_intf))
  > EOF
  $ cat > "foo_intf/foo_intf.mli" <<EOF
  > val x : int
  > EOF
  $ make_dir_with_dune "foo_impl" <<EOF
  > (library
  >  (name foo_impl)
  >  (implements foo_intf))
  > EOF
  $ cat > "foo_impl/foo_impl.ml" <<EOF
  > let x = 42
  > EOF

Creates a parametrized library using the parameter we define earlier.

  $ make_lib_parameterized "foo_func" "foo_intf"
  $ cat > "foo_func/foo_func.ml" <<EOF
  > let x = Foo_func_aux.x
  > EOF
  $ cat > "foo_func/foo_func_aux.ml" <<EOF
  > let x = Foo_intf.x
  > EOF

  $ make_dir_with_dune "foo_func_2" <<EOF
  > (library
  >  (name foo_func_2)
  >  (parameters foo_intf)
  >  (libraries foo_func))
  > EOF
  $ cat > "foo_func_2/foo_func_2.ml" <<EOF
  > let x = Foo_func_2_aux.x
  > EOF
  $ cat > "foo_func_2/foo_func_2_aux.ml" <<EOF
  > let x = Foo_intf.x + Foo_func.x
  > EOF

  $ make_dir_with_dune "bin" <<EOF
  > (executable
  >  (name main)
  >  (libraries (foo_func_2 foo_impl) :as foo_inst))
  > EOF

TODO(@maiste): remove /dev/null
FIXME: you should be able to use instantiate and also the parameter should be provided transitively.
  $ dune build > /dev/null
  File "bin/dune", line 3, characters 12-33:
  3 |  (libraries (foo_func_2 foo_impl) :as foo_inst))
                  ^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]
