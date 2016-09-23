{ mkDerivation, base, containers, ghcjs-base, ghcjs-dom, miso, stdenv, text,
  closurecompiler
}:
mkDerivation {
  pname = "product-chart-demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers ghcjs-base ghcjs-dom miso text
  ];
  license = stdenv.lib.licenses.unfree;
}
