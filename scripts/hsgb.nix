{ mkDerivation, base, heredoc, hspec, parsec, QuickCheck, sdl2, stdenv, transformers, vector, cabal-install }:
mkDerivation {
  pname = "gameboy";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ base parsec sdl2 transformers vector ];
  testDepends = [ base heredoc hspec QuickCheck transformers ];
  buildTools = [ cabal-install ];
  license = stdenv.lib.licenses.publicDomain;
}
