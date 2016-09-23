Product Chart Demo
==================

Building
--------

You need

  1. [nix](https://nixos.org/nix/)
  2. Miso (unreleased by author)

To build

  1. Set up `default.nix` to point to your checkout of Miso.
  2. Go to the repository directory.
  3. `nix-shell`
  4. `cabal configure --ghcjs`
  5. `make`
  6. Results will be in `out`.

For production:

  1. `make prod`
  2. Results will be in `out`.
