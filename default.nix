{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc784" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./hsgb.nix { }
