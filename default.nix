{ mkDerivation, base, Cabal, containers, directory, filepath
, hackage-db, process, stdenv, temporary
}:
mkDerivation {
  pname = "hackage-whatsnew";
  version = "0.1.0.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Cabal containers directory filepath hackage-db process
    temporary
  ];
  homepage = "https://github.com/stepcut/hackage-whatsnew";
  description = "Check for differences between working directory and hackage";
  license = stdenv.lib.licenses.bsd3;
}
