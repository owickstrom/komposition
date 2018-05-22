{ mkDerivation, base, indexed, indexed-extras, reflection
, row-types, stdenv, template-haskell
}:
mkDerivation {
  pname = "motor";
  version = "0.3.0";
  sha256 = "70256587315562821abff7fab264935838cf4ee165bef89e2d17798b1b7fa07b";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base indexed indexed-extras reflection row-types template-haskell
  ];
  executableHaskellDepends = [
    base indexed indexed-extras row-types
  ];
  testHaskellDepends = [ base indexed indexed-extras row-types ];
  description = "Type-safe effectful state machines in Haskell";
  license = stdenv.lib.licenses.mpl20;
}
