{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellLib = nixpkgs.haskell.lib;

  haskellPackages = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      hweblib = haskellLib.dontCheck super.hweblib;

      # Copied form nixpkgs master @ 2018-04-30
      "hnix" = self.callPackage
        ({ mkDerivation, ansi-wl-pprint, base, containers, criterion
         , data-fix, deepseq, deriving-compat, parsers, regex-tdfa
         , regex-tdfa-text, semigroups, tasty, tasty-hunit, tasty-th, text
         , transformers, trifecta, unordered-containers
         }:
         mkDerivation {
           pname = "hnix";
           version = "0.4.0";
           sha256 = "0rgx97ckv5zvly6x76h7nncswfw0ik4bhnlj8n5bpl4rqzd7d4fd";
           isLibrary = true;
           isExecutable = true;
           libraryHaskellDepends = [
             ansi-wl-pprint base containers data-fix deepseq deriving-compat
             parsers regex-tdfa regex-tdfa-text semigroups text transformers
             trifecta unordered-containers
           ];
           executableHaskellDepends = [
             ansi-wl-pprint base containers data-fix deepseq
           ];
           testHaskellDepends = [
             base containers data-fix tasty tasty-hunit tasty-th text
           ];
           benchmarkHaskellDepends = [ base containers criterion text ];
           homepage = "http://github.com/jwiegley/hnix";
           description = "Haskell implementation of the Nix language";
           license = nixpkgs.stdenv.lib.licenses.bsd3;
           hydraPlatforms = nixpkgs.stdenv.lib.platforms.none;
         }) {};
    };
  };

in haskellPackages.callPackage ./default.nix {}
