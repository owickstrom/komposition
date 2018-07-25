{ compiler ? "ghc843" }:
(import ./. { inherit compiler; }).fastcut-shell
