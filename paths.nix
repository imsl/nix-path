let

  nixpkgs-master = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";

  nixpkgs-1509 = "https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz";

in {
  nixpkgs = nixpkgs-1509;

  # Unfortunately, hnix does not yet support 'inherit'
  nixpkgs-master = nixpkgs-master;
  nixpkgs-1509 = nixpkgs-1509;
}
