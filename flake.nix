{
  description = "A very basic flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages // {
          annodate = prev.haskellPackages.callPackage ./. { };
        };
      };
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ overlay ];
      };
      inherit (pkgs.haskell.lib.compose) justStaticExecutables;
    in {

      inherit overlay;

      defaultPackage.${system} =
        justStaticExecutables pkgs.haskellPackages.annodate;

      devShell.${system} = pkgs.haskellPackages.shellFor {
        withHoogle = true;
        packages = _: [ pkgs.haskellPackages.annodate ];
        buildInputs =
          (with pkgs; [ cabal-install haskell-language-server hlint ])
          ++ (with pkgs.haskellPackages; [ ghcid ]);
      };

    };
}
