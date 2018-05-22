{ mkDerivation, base, bytestring, criterion, deepseq, directory
, ffmpeg-light, filepath, gi-gdk, gi-glib, gi-gobject, gi-gtk
, gi-pango, hashable, haskell-gi, haskell-gi-base, indexed
, JuicyPixels, lens, motor, mtl, parallel, pipes, pipes-extras
, row-types, stdenv, tasty, tasty-discover, tasty-hspec
, tasty-hunit, text, time, unordered-containers, vector
}:
mkDerivation {
  pname = "fastcut";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base deepseq directory ffmpeg-light filepath gi-gdk gi-glib
    gi-gobject gi-gtk gi-pango hashable haskell-gi haskell-gi-base
    indexed JuicyPixels lens motor mtl parallel pipes pipes-extras
    row-types text time unordered-containers vector
  ];
  executableHaskellDepends = [
    base gi-gdk gi-glib gi-gobject gi-gtk gi-pango haskell-gi
    haskell-gi-base text time
  ];
  testHaskellDepends = [
    base tasty tasty-discover tasty-hspec tasty-hunit
    unordered-containers
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion JuicyPixels lens vector
  ];
  description = "High-productivity video and audio editing";
  license = stdenv.lib.licenses.mpl20;
}
