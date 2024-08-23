{
  lib,
  mkDerivation,
  nix-gitignore,
  ansi-terminal,
  old-locale,
  optparse-applicative,
  process,
  random,
  text,
  time,
}:

let
  gitignore = nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

in mkDerivation {
  pname = "annodate";
  version = "0.1.0.0";
  src = gitignore ./.;
  isExecutable = true;
  executableHaskellDepends = [
    ansi-terminal
    old-locale
    optparse-applicative
    process
    random
    text
    time
  ];
  license = lib.licenses.mit;
}
