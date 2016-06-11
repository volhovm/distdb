{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-6_1" }:

let
  inherit (nixpkgs) pkgs;
  f = { mkDerivation, stdenv }:
      mkDerivation {
        pname = "rscoin";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryPkgconfigDepends = 
          with nixpkgs; 
          [ zlib autoreconfHook pkgconfig ];
        license = stdenv.lib.licenses.gpl3;
      };
  haskellPackages =  pkgs.haskell.packages.${compiler};
  drv = haskellPackages.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv
