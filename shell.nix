{ pkgs ? import ./nixpkgs.nix {}, }:

let
  drv = pkgs.callPackage ./default.nix {};

in pkgs.haskellPackages.shellFor {
  withHoogle = true;
  packages = _: [ drv ];
  buildInputs = (with pkgs; [
    cabal-install
    hlint
  ]) ++ (with pkgs.haskellPackages; [
    ghcid
  ]);
}
