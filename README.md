# Probidor

# Building and running

To create a local opam switch (assuming opam is installed) : 
$ opam switch create . 5.1.1

To install the dependencies (assuming opam is installed) : 
$ opam install . --deps-only

To build : 
$ dune build

To run the tests : 
$ dune runtest

To run an executable bin/my_executable.ml :
$ dune exec bin/my_executable.exe

To format the code :
$ dune fmt