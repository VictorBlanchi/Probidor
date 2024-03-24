# Probidor

# Building and running

To install the dependencies (assuming opam is installed) : 
$ opam install . --deps-only

To build : 
$ dune build

To run the tests : 
$ dune runtest

To run an executable bin/my_executable.ml :
$ dune exec bin/my_executable.exe