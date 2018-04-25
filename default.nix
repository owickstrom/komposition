{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, gi-gdk, gi-glib, gi-gobject, gi-gtk
      , gi-pango, haskell-gi, haskell-gi-base, mtl, stdenv, tasty
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
          base gi-gdk gi-glib gi-gobject gi-gtk gi-pango haskell-gi
          haskell-gi-base mtl text time unordered-containers
        ];
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
        preCompileBuildDriver = ''
          PKG_CONFIG_PATH+=":${pkgs.cairo}/lib/pkgconfig"
          setupCompileFlags+=" $(pkg-config --libs cairo-gobject)"
        '';
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
