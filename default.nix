{
  pkgs ? import ./nixpkgs.nix {},
  haskellPackages ? pkgs.haskellPackages,
}:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  src = gitignore ./.;

in haskellPackages.mkDerivation {
  inherit src;
  pname = "annodate";
  version = "0.1.0.0";
  # isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = with haskellPackages; [
    ansi-terminal
    old-locale
    optparse-applicative
    process
    text
    time
  ];
  license = pkgs.lib.licenses.mit;
}
