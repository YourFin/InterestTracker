{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell { buildInputs = [ haskellPackages.stack ]; }
