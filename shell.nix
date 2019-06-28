{ compiler ? "ghc864" }:
(import ./. { inherit compiler; doBenchmark = true; }).komposition-shell
