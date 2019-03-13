#!/bin/bash

# Install odoc
opam depext -y odoc
cd /home/opam
opam pin add odoc odoc -k git -y

env

# Dependencies for odoc-test
opam install num ocaml-compiler-libs

mkdir /tmp/build
cd /tmp/build
git clone https://github.com/jonludlam/odoc-test.git
cd odoc-test
git submodule init
git submodule update
dune build @doc

# tidy
apt install tidy
cd /tmp/build/odoc-test/_build/default/_doc/_html
rsync -avz --delete . ~/odoc-test-output/
cd ~/odoc-test-output
for i in `find . -name "*.html"`; do tidy -m $i; done
git add .
git commit -m "Automatic update"
git show


