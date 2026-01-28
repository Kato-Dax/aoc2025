{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs.buildPackages; [
      guile
    ];
    buildInputs = with pkgs; [
      raylib
    ];
  }

