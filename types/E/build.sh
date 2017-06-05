#!/bin/bash

ocamlfind opt -o testE -package oUnit -linkpkg -g Untyped.ml Typed.ml test.ml
