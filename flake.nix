{
  description = "Csv Parser";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
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

        haskellPackages = pkgs.haskellPackages;
      in
      {
        # packages.${packageName} = haskellPackages.callCabal2nix packageName self rec {
        #   # Dependency overrides go here
        # };

        # packages.default = self.packages.${system}.${packageName};
        # defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
        };
        devShell = self.devShells.${system}.default;
      }
    );
}
