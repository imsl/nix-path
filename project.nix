{ mkDerivation, attoparsec, base, bytestring, containers, data-fix
, directory, filepath, hnix, hweblib, process, split, stdenv, text
, unix, uuid, xdg-basedir
}:
mkDerivation {
  pname = "nix-path";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring containers data-fix directory filepath
    hnix hweblib process split text unix uuid xdg-basedir
  ];
  description = "nix-path";
  license = stdenv.lib.licenses.unfree;
}
