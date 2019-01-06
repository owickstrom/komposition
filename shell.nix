{ compiler ? "ghc863" }:
(import ./. { inherit compiler; doBenchmark = true; }).komposition-shell
