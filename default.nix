{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
    let
       pkgs = nixpkgs.pkgs.haskell.packages.${compiler};
    in
      pkgs.callPackage ./product-chart-demo.nix {
          miso = pkgs.callPackage /Users/elliot/a/mine/miso/miso.nix {};
      }
