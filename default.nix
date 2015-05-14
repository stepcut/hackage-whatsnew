{ mkDerivation, base, Cabal, containers, directory, filepath
, hackage-db, process, stdenv, temporary, cabal-install, ghc
, makeWrapper
}:
mkDerivation rec {
  pname = "hackage-whatsnew";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base Cabal containers directory filepath hackage-db process
    temporary
  ];
  buildTools = [ makeWrapper ];
  postInstall = ''
    exe=$out/libexec/${pname}-${version}/hackage-whatsnew
    install -D $out/bin/hackage-whatsnew $exe
    rm -rf $out/{bin,lib,share}
    makeWrapper $exe $out/bin/hackage-whatsnew --prefix PATH ":" "${cabal-install}/bin:${ghc}/bin"
  '';
  license = stdenv.lib.licenses.bsd3;
}
