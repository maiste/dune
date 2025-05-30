Test that implementations can use c stubs.

  $ . ./helpers.sh
  $ init_project

  $ make_dir_with_dune "param_intf" <<EOF
  > (library_parameter
  >  (name param_intf))
  > EOF
  $ cat > "param_intf/param_intf.mli" <<EOF
  > type t
  > val to_string : t -> string
  > EOF

  $ make_dir_with_dune "param_impl" << EOF
  > (library
  >  (name param_impl)
  >  (implements param_intf)
  >  (foreign_stubs (language c) (names stub)))
  > EOF
  $ cat > "param_impl/param_impl.ml" <<EOF
  > type t = int
  > let to_string = string_of_int
  > external increment : t -> t = "caml_increment"
  > EOF
  $ cat > "param_impl/stub.c" <<EOF
  > #include <caml/mlvalues.h>
  > #include <caml/memory.h>
  > CAMLprim value caml_increment (value n) {
  >   CAMLparam1(n);
  >   int result = Int_val(n) + 1;
  >  CAMLreturn(Val_int(result));
  > }
  > EOF

  $ make_dir_with_dune "print_func" <<EOF
  > (library
  >   (name print_func)
  >   (parameters param_intf))
  > EOF
  $ cat > "print_func/print_func.ml" <<EOF
  > let print n = print_endline (Param_intf.to_string n)
  > EOF
  > cat > "print_func/print_func.mli" <<EOF
  > val print : Param_intf.t -> unit
  > EOF

  $ make_dir_with_dune "lib" <<EOF
  > (library
  >  (name lib)
  >  (libraries ((print_func param_impl) :as print_param) param_impl)
  >  (foreign_stubs (language c) (names stub)))
  > EOF
  $ cat > "lib/lib.ml" <<EOF
  > external add_two : int -> int = "caml_add_two"
  > let three () = Print_param.print (add_two (Param_impl.increment 0))
  > EOF
  $ cat > "lib/stub.c" <<EOF
  > #include <caml/mlvalues.h>
  > #include <caml/memory.h>
  > CAMLprim value caml_add_two (value n) {
  >   CAMLparam1(n);
  >   int result = Int_val(n) + 2;
  >  CAMLreturn(Val_int(result));
  > }
  > EOF

  $ make_dir_with_dune "bin" <<EOF
  > (executable
  >  (name main)
  >  (libraries lib))
  > EOF
  $ cat > "bin/main.ml" <<EOF
  > let () = Lib.three ()
  > EOF

  $ dune exec -- ./bin/main.exe
  File "lib/dune", line 3, characters 12-53:
  3 |  (libraries ((print_func param_impl) :as print_param) param_impl)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]
