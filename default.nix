{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
    let
       pkgs = nixpkgs.pkgs.haskell.packages.${compiler};
       misoRepo = /Users/elliot/a/mine/miso;
       miso = misoRepo + "/miso/miso.nix";
       misoHtml = misoRepo + "/miso-html/miso-html.nix";
    in
      pkgs.callPackage ./product-chart-demo.nix {
          miso = pkgs.callPackage miso {
	    miso-html = pkgs.callPackage misoHtml {};
	  };
          miso-html = pkgs.callPackage misoHtml {};
      }

