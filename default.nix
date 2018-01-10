{ mkDerivation, async, attoparsec, base, bytestring, containers
, data-fix, directory, dirstream, filepath, hnix, hweblib, pipes
, pipes-concurrency, pipes-safe, process, random, split, stdenv
, system-filepath, temporary, text, unix, uuid, xdg-basedir, xxhash
}:
mkDerivation {
  pname = "nix-path";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers data-fix filepath hnix
    hweblib text unix
  ];
  executableHaskellDepends = [
    async base bytestring containers directory dirstream filepath
    hweblib pipes pipes-concurrency pipes-safe process random split
    system-filepath temporary text unix uuid xdg-basedir xxhash
  ];
  description = "nix-path";
  license = stdenv.lib.licenses.mit;
}
