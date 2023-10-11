{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
    node2nix-hs.url = "github:ptitfred/node2nix-hs/v0.0.1.0";
  };

  outputs = { nixpkgs, flake-utils, gitignore, easy-purescript-nix, node2nix-hs, ... }:
    let frontendOverlay = _: prev: {
          inherit (gitignore.lib) gitignoreSource;
          easy-ps = easy-purescript-nix.packages.${prev.system};
        };
        overlays = [ node2nix-hs.overlays.default frontendOverlay ];
     in flake-utils.lib.eachDefaultSystem (system:
          let pkgs = import nixpkgs { inherit system overlays; };
              pname = "purescript-mantine"; version = "0.1.0"; src = ./.;

           in {
                packages.default  = pkgs.callPackage ./package.nix { inherit pname version src; };
                devShells.default = pkgs.callPackage ./shell.nix   { inherit pname version; };
              }
        );
}
