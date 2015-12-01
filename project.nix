{ mkDerivation, attoparsec, base, bytestring, containers, data-fix
, filepath, hnix, hweblib, stdenv, text, unix
}:
mkDerivation {
  pname = "nix-path";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring containers data-fix filepath hnix
    hweblib text unix
  ];
  description = "nix-path";
  license = stdenv.lib.licenses.unfree;
}
