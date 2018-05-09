let overlay = self: super: {
  all-cabal-hashes = self.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/5ed06084aa1933c8131bb674b5de86bbb977c5f0.tar.gz";
    sha256 = "06nva1c2wj7z8pagchlc5ax3rhb9mgc9myxh9k07p9vyi7s10zrk";
  };
  haskellPackages = super.haskellPackages.extend (sel: sup: {
    haskell-gi = sel.callHackage "haskell-gi" "0.21.2" {};
    haskell-gi-base = self.haskell.lib.addBuildDepend (sel.callHackage "haskell-gi-base" "0.21.1" {}) self.pkgs.gobjectIntrospection;
    haskell-gi-overloading = self.haskell.lib.dontHaddock (sel.callHackage "haskell-gi-overloading" "0.0" {});
  });
};
in
{ nixpkgs ? import <nixpkgs> { overlays = [ overlay ]; }, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, gi-gdk, gi-glib, gi-gobject, gi-gtk
      , gi-pango, hashable, haskell-gi, haskell-gi-base, mtl, stdenv, tasty
      , tasty-discover, tasty-hspec, tasty-hunit, text, time
      , unordered-containers
      }:
      mkDerivation {
        pname = "fastcut";
        version = "0.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          base gi-gdk gi-glib gi-gobject gi-gtk gi-pango hashable haskell-gi
          haskell-gi-base mtl text time unordered-containers
        ];
        librarySystemDepends = [ pkgs.pkgconfig pkgs.cairo ];
        executableHaskellDepends = [
          base gi-gdk gi-glib gi-gobject gi-gtk gi-pango haskell-gi
          haskell-gi-base text time
        ];
        testHaskellDepends = [
          base tasty tasty-discover tasty-hspec tasty-hunit
          unordered-containers
        ];
        description = "High-productivity video and audio editing";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
