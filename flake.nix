{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            OrderOrder = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              index-state = "2022-06-01T00:00:00Z";
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
      in {
        packages.default = pkgs;
        apps.default = {
          type = "app";
          program = "${
              pkgs.OrderOrder.getComponent "OrderOrder:exe:orderorder"
            }/bin/orderorder";
        };
      });
}
