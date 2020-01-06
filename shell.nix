{ compiler ? "ghc865" }:
(import ./. { inherit compiler; doBenchmark = true; }).komposition-shell
