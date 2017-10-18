{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:

let

  haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler};

  haskellLib = nixpkgs.haskell.lib;

  eval_env_path_patch = nixpkgs.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/jwiegley/hnix/pull/66.patch";
    sha256 = "05w440xmdiz9syadbnclwk45jxpvbyzm5vwiiaw88yl16m5w1qm0";
  };

  hnix = haskellLib.appendPatch haskellPackages.hnix eval_env_path_patch;

in haskellPackages.callPackage ./project.nix { inherit hnix; }
