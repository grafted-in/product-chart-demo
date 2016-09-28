{ mkDerivation, stdenv, base, containers, ghcjs-base, ghcjs-dom, text, miso, miso-html, closurecompiler }:
mkDerivation {
  pname = "product-chart-demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers ghcjs-base ghcjs-dom miso miso-html text
  ];
  license = stdenv.lib.licenses.unfree;
}
