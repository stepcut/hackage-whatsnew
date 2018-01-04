with (import <nixpkgs> {}).pkgs;
let pkg = haskellPackages.callPackage
            ({ mkDerivation, base, Cabal, containers, stdenv, hackage-db, process, cabal-install, coreutils, temporary }:
             mkDerivation {
               pname = "cabal-needs-upload";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [ base Cabal containers hackage-db process temporary cabal-install ];
               buildTools = [ cabal-install coreutils ];
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
