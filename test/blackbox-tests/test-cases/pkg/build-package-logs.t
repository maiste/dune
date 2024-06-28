Test the error message when installing package that fails.

  $ . ./helpers.sh
  $ make_lockdir

Make a project with two packages, one successful and, one that fails:

  $ cat > dune-project << EOF
  > (lang dune 3.12)
  > EOF

We create a package with a failing command that throws an error:

  $ make_lockpkg x << EOF
  > (version 0.0.1)
  > (build 
  >    (progn
  >       (run cat i_dont_exist)))
  > EOF

Building the package should fail and print an error:

  $ build_pkg x
  File "dune.lock/x.pkg", line 4, characters 11-14:
  4 |       (run cat i_dont_exist)))
                 ^^^
  Package x fails to build
  /usr/bin/cat: i_dont_exist: No such file or directory
  
  [1]

We create a package with a succeeding command that displays some text:

  $ make_lockpkg y << EOF
  > (version 0.0.1)
  > (build 
  >    (progn
  >       (run echo "Success!")
  >       (run true)))
  > EOF

Building the package should succeed and print no output:

  $ build_pkg y
