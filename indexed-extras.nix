{ mkDerivation, base, bifunctors, fetchgit, indexed, mtl, pointed
, stdenv
}:
mkDerivation {
  pname = "indexed-extras";
  version = "0.2";
  src = fetchgit {
    url = "https://github.com/typedrat/indexed-extras";
    sha256 = "06g65yaf1hj6fbfisq134g6z757b6qgg7bg0am9n6ksznkvy69fz";
    rev = "b54bdf861468d7bb9e045a3d3b53ed7ef34e3ecc";
  };
  libraryHaskellDepends = [ base bifunctors indexed mtl pointed ];
  homepage = "https://github.com/reinerp/indexed-extras";
  description = "Indexed functors, monads and comonads that require extensions to Haskell98";
  license = stdenv.lib.licenses.bsd3;
}
