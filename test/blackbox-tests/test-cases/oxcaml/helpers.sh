export XDG_CACHE_HOME="$PWD/.cache"

init_project() {
  echo "(lang dune 3.19)" >> "dune-project"
}

create_dune_dir() {
  path="$1"
  mkdir -p $path
  cat >> "$path/dune"
}

clean_build() {
  rm -rf _build
}
