{
  description = "A very basic flake";

  outputs = { self, nixpkgs }:
  let system = "x86_64-linux";
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages // {
          annodate = prev.callPackage ./. {};
        };
      };
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ overlay ];
      };
  in {

    inherit overlay;

    defaultPackage.${system} = pkgs.haskellPackages.annodate;

    devShell.${system} = pkgs.haskellPackages.shellFor {
      withHoogle = true;
      packages = _: [ pkgs.haskellPackages.annodate ];
      buildInputs = (with pkgs; [
        cabal-install
        haskell-language-server
        hlint
      ]) ++ (with pkgs.haskellPackages; [
        ghcid
      ]);
    };

  };
}
