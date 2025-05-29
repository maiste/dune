Test instantiation with parameterized libraries.

  $ . ./helpers.sh
  $ init_project

We create two parameters to be used by libraries.

  $ make_dir_with_dune "p1_intf" <<EOF
  > (library_parameter
  >  (name p1_intf))
  > EOF
  $ cat > "p1_intf/p1_intf.mli" <<EOF
  > val x : unit -> string
  > EOF

  $ make_dir_with_dune "p2_intf" <<EOF
  > (library_parameter
  >  (name p2_intf))
  > EOF
  $ cat > "p2_intf/p2_intf.mli" <<EOF
  > val x : unit -> string
  > EOF

We create implementation of the parameters.

  $ make_dir_with_dune "dep" <<EOF
  > (library
  >   (name dep))
  > EOF
  $ cat > "dep/dep.ml" <<EOF
  > let x () = __FILE__
  > EOF

  $ make_dir_with_dune "p1_impl" <<EOF
  > (library
  >  (name p1_impl)
  >  (libraries dep)
  >  (implements p1_intf))
  > EOF
  $ cat > "p1_impl/p1_impl.ml" <<EOF
  > let x () = P1_impl_aux.x () ^ " " ^ __FILE__
  > EOF
  $ cat > "p1_impl/p1_impl_aux.ml" <<EOF
  > let x () = Dep.x () ^ " " ^ __FILE__
  > EOF

  $ make_dir_with_dune "p2_impl" <<EOF
  > (library
  >  (name p2_impl)
  >  (libraries dep)
  >  (implements p2_intf))
  > EOF
  $ cat > "p2_impl/p2_impl.ml" <<EOF
  > let x () = P2_impl_aux.x () ^ " " ^ __FILE__
  > EOF
  $ cat > "p2_impl/p2_impl_aux.ml" <<EOF
  > let x () = Dep.x () ^ " " ^ __FILE__
  > EOF

We create a library parameterized with [p1_intf].

  $ make_dir_with_dune "p1_func" <<EOF
  > (library
  >  (name p1_func)
  >  (parameters p1_intf))
  > EOF
  $ cat > "p1_func/p1_func_aux.ml" <<EOF
  > let x () = P1_intf.x () ^ " " ^ __FILE__
  > EOF
  $ cat > "p1_func/p1_func.ml" <<EOF
  > let x () = P1_func_aux.x () ^ " " ^ __FILE__
  > EOF

We instantiate an executable using it.

  $ make_dir_with_dune "bin" <<EOF
  > (executable
  >  (name main)
  >  (libraries ((p1_func p1_impl) :as lib1)))
  > EOF
  $ cat > "bin/main.ml" <<EOF
  > let () = print_endline (Lib1.x ())
  > EOF

FIXME: we should be able to use instantiate and see the files.
  $ dune exec -- ./bin/main.exe
  File "bin/dune", line 3, characters 12-40:
  3 |  (libraries ((p1_func p1_impl) :as lib1)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

We instantiate an executable depending on an instantiation indirectly, via
another library.

  $ make_dir_with_dune "lib" <<EOF
  > (library
  >  (name lib)
  >  (libraries ((p1_func p1_impl) :as lib1)))
  > EOF
  $ cat > "lib/lib.ml" <<EOF
  > let x () = Lib1.x () ^ " " ^ __FILE__
  > EOF

  $ rm -rf _build bin
  $ make_dir_with_dune "bin" <<EOF
  > (executable
  >  (name main)
  >  (libraries lib))
  > EOF
  $ cat > "bin/main.ml" <<EOF
  > let () = print_endline (Lib.x ())
  > EOF

FIXME: we should be able to use instantiate and see the files.
  $ dune exec -- ./bin/main.exe
  File "lib/dune", line 3, characters 12-40:
  3 |  (libraries ((p1_func p1_impl) :as lib1)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

We instantiate an executable depending on the same parametrized library but
with two different implementations.

  $ rm -rf _build bin lib
  $ make_dir_with_dune "p1_impl_2" <<EOF
  > (library
  >  (name p1_impl_2)
  >  (implements p1_intf))
  > EOF
  $ cat > "p1_impl_2/p1_impl_2.ml" <<EOF
  > let x () = __FILE__
  > EOF

  $ make_dir_with_dune "bin" <<EOF
  > (executable
  >  (name main)
  >  (libraries 
  >   ((p1_func p1_impl) :as instance1)
  >   ((p1_func p1_imp_2) :as instance2)))
  > EOF
  $ cat > "bin/main.ml" <<EOF
  > let () = print_endline (Instance1.x()); print_endline (Instance2.x ())
  warning: here-document at line 1 delimited by end-of-file (wanted `EOF')

FIXME: support implementation
  $ dune exec -- ./bin/main.exe
  File "bin/dune", line 4, characters 2-35:
  4 |   ((p1_func p1_impl) :as instance1)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]


Partial implementation of the a parametrized library.

  $ rm -rf bin p1_impl_2

  $ make_dir_with_dune "p1_2_func" <<EOF
  > (library
  >   (name p1_2_func)
  >   (parameters p1_intf p2_intf))
  > EOF
  $ cat > "p1_2_func/p1_2_func" <<EOF
  > let y () = "(" ^ P1_intf.x () ^ ", " P2_intf.x () ^ ")"
  > EOF

  $ make_dir_with_dune "p1_2_impl_part" <<EOF
  > (library
  >  (name p1_2_impl_part)
  >  (parameters p2_intf)
  >  (libraries ((p1_2_func p1_impl) :as inst)))
  > EOF
  $ cat > "p1_2_impl_part/p1_2_impl_part.ml" <<EOF
  > let y () = Inst.y () ^ " " ^ P2.intf.x ()
  > EOF

  $ make_dir_with_dune "p1_2_impl" <<EOF
  > (library
  >  (name p1_2_impl)
  >  (libraries ((p1_2_impl_part p2_impl) :as inst)))
  > EOF
  $ cat > "p1_2_impl/p1_2_impl.ml" <<EOF
  > let y () = Inst.y ()
  > EOF

  $ make_dir_with_dune "bin" <<EOF
  > (executable
  >  (name main)
  >  (libraries p1_2_impl))
  > EOF
  $ cat > "bin/main.ml" <<EOF
  > let  () = P1_2_impl.y ()
  > EOF

  $ dune exec -- ./bin/main.exe
  File "p1_2_impl/dune", line 3, characters 12-47:
  3 |  (libraries ((p1_2_impl_part p2_impl) :as inst)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  File "p1_2_impl_part/dune", line 4, characters 12-42:
  4 |  (libraries ((p1_2_func p1_impl) :as inst)))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]
