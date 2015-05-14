{ mkDerivation, base, Cabal, containers, directory, filepath
, hackage-db, process, stdenv, temporary
}:
mkDerivation {
  pname = "hackage-whatsnew";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base Cabal containers directory filepath hackage-db process
    temporary
  ];
  license = stdenv.lib.licenses.bsd3;
}
