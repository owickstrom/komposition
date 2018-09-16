{ compiler ? "ghc843" }:
(import ./. { inherit compiler; doBenchmark = true; }).fastcut-shell
