export XDG_CACHE_HOME="$PWD/.cache"

init_project() {
  echo "(lang dune 3.19)" >> "dune-project"
}

make_dir_with_dune() {
  path="$1"
  mkdir -p $path
  cat > "$path/dune"
}

make_dummy_intf() {
  dir="$1"
  name="$2"
  cat >> "$dir/$name.mli" <<EOF
type t
val f : t -> unit
EOF
}

make_dummy_impl() {
  dir="$1"
  name="$2"
  cat >> "$dir/$name.ml" <<EOF
type t = int
let f _ = ()
EOF
}

make_lib_impl() {
  name="$1"
  implements="$2"
  make_dir_with_dune $name <<EOF
(library
  (name $name)
  (implements $implements))
EOF

  make_dummy_impl "$name" "$name"
}

make_lib_parameterized() {
  name="$1"
  parameter="$2"
  make_dir_with_dune $name <<EOF
(library
  (name $name)
  (parameters $parameter))
EOF
}

target_cmi() {
  echo "./$1/.$1.objs/byte/$1.cmi"
}



