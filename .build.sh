#!/bin/bash

# Install odoc
opam depext -y odoc
cd /home/opam
opam pin add odoc odoc -k git -y

# Dependencies for odoc-test
opam install num ocaml-compiler-libs

mkdir /tmp/build
cd /tmp/build
git clone https://github.com/jonludlam/odoc-test.git
cd odoc-test
git submodule init
git submodule update
dune build @doc

