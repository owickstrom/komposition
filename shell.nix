{ compiler ? "ghc862" }:
(import ./. { inherit compiler; doBenchmark = true; }).komposition-shell
