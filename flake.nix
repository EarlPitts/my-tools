{
  description = "Toolchains";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskell = pkgs.haskellPackages;
        toolchain = with pkgs; [
          # Haskell
          haskell.ghc
          haskell.cabal-install
          haskell.haskell-language-server
          haskell.hlint
          haskell.cabal-fmt
        ];

      in
      {
        devShells = {
          default = pkgs.mkShell {
            packages = toolchain;
            buildInputs = [
              pkgs.zlib
            ];
          };
        };
      }
    );
}
