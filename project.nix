{ mkDerivation, base, containers, data-fix, filepath, hnix, split
, stdenv, text, unix
}:
mkDerivation {
  pname = "nix-path";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers data-fix filepath hnix split text unix
  ];
  description = "nix-path";
  license = stdenv.lib.licenses.unfree;
}
