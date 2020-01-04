{ compiler ? "ghc865", doCheck ? true, doBenchmark ? false }:

let
  nixpkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2019-06-23";
    url = https://github.com/nixos/nixpkgs-channels/;
    rev = "3ddd23719bbd878c5ddf4ad9597c0e00904245f8";
    ref = "nixos-19.03";
  }) {};

  inherit (nixpkgs) pkgs;

  githubHaskellPackage = pkgs: attrs: args:
    let
      prefetched = builtins.fromJSON (builtins.readFile attrs.path);
      src = nixpkgs.fetchFromGitHub {
        owner = attrs.owner;
        repo = attrs.repo;
        inherit (prefetched) rev sha256;
      };
      packageSrc = if attrs.repoSubDir == "" then src else "${src}/${attrs.repoSubDir}";
    in pkgs.callCabal2nix attrs.repo packageSrc args;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      ffmpeg-light = pkgs.haskell.lib.doJailbreak (self.callHackage "ffmpeg-light" "0.12.2.2" {
        libavcodec = null;
        libavdevice = null;
        libavformat = null;
        libswscale = null;
      });
      fused-effects = githubHaskellPackage self {
        owner = "robrix";
        repo = "fused-effects";
        path = nix/fused-effects.json;
        repoSubDir = "";
      } {};
      hedgehog = githubHaskellPackage self {
        owner = "hedgehogqa";
        repo = "haskell-hedgehog";
        path = nix/haskell-hedgehog.json;
        repoSubDir = "hedgehog";
      } {};
      indexed-extras = pkgs.haskell.lib.doJailbreak super.indexed-extras;
      motor = githubHaskellPackage self {
        owner = "owickstrom";
        repo = "motor";
        path = nix/motor.json;
        repoSubDir = "motor";
      } {};
      pipes-safe = self.callHackage "pipes-safe" "2.3.1" {};
      protolude = self.callHackage "protolude" "0.2.3" {};
      gi-gtk-declarative =
        let
          prefetched = builtins.fromJSON (builtins.readFile ./nix/gi-gtk-declarative.json);
          src = nixpkgs.fetchFromGitHub {
            owner = "owickstrom";
            repo = "gi-gtk-declarative";
            inherit (prefetched) rev sha256;
          };
        in pkgs.haskell.lib.dontCheck
          (self.callCabal2nix "gi-gtk-declarative" "${src}/gi-gtk-declarative" {});
      row-types = githubHaskellPackage self {
        owner = "target";
        repo = "row-types";
        path = nix/row-types.json;
        repoSubDir = "";
      } {};
      tasty-hedgehog = githubHaskellPackage self {
        owner = "qfpl";
        repo = "tasty-hedgehog";
        path = nix/tasty-hedgehog.json;
        repoSubDir = "";
      } {};
    };
  };
  fontsConf = nixpkgs.makeFontsConf {
    fontDirectories = [ nixpkgs.cantarell-fonts ];
  };
  toggleCheck = if doCheck then pkgs.haskell.lib.doCheck else pkgs.haskell.lib.dontCheck;
  toggleBenchmark = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  withBuildInputs = extraInputs: d:
    pkgs.lib.overrideDerivation d (old: {
      buildInputs = old.buildInputs ++ extraInputs;
    });

  drv = withBuildInputs
        [pkgs.ffmpeg]
        (toggleBenchmark
         (toggleCheck
          (pkgs.haskell.lib.justStaticExecutables
          (pkgs.haskell.lib.dontHaddock (haskellPackages.callCabal2nix "komposition" ./. {})))));

  python = import ./docs/requirements.nix { pkgs = nixpkgs; };

  komposition = nixpkgs.stdenv.mkDerivation {
    name = "komposition";
    nativeBuildInputs = with nixpkgs; [ makeWrapper ];
    buildInputs = with nixpkgs; [
      wrapGAppsHook
      gnome3.gtk
      gnome3.dconf
      gnome3.defaultIconTheme
      gnome3.gsettings_desktop_schemas
      gst_all_1.gstreamer
      gst_all_1.gst-plugins-base
      gst_all_1.gst-plugins-good
      (gst_all_1.gst-plugins-good.override { gtkSupport = true; })
      gst_all_1.gst-libav
    ];
    nativeCheckInputs = [];
    checkInputs = [nixpkgs.ffmpeg];
    src = ./.;
    buildPhase = ''
        mkdir -p $out
      '';
    installPhase = ''
      mkdir -p $out/bin
      ln -s ${drv}/bin/komposition $out/bin
      wrapProgram $out/bin/komposition \
        --set FONTCONFIG_FILE "${fontsConf}" \
        --prefix 'PATH' ':' "${pkgs.ffmpeg}/bin" \
        --prefix 'PATH' ':' "${pkgs.sox}/bin"
    '';
  };
  shell = haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [drv];
    buildInputs = with pkgs; [
      cabal-install
      # Gnome/GTK+/Gstreamer
      gnome3.gtk
      gnome3.dconf
      gnome3.defaultIconTheme
      gnome3.gsettings_desktop_schemas
      gst_all_1.gstreamer
      gst_all_1.gst-plugins-base
      (gst_all_1.gst-plugins-good.override { gtkSupport = true; })
      gst_all_1.gst-libav
      # CLI tools
      ffmpeg
      sox
      # Documentation
      python.packages.mkdocs
      python.packages.mkdocs-material
      ];
    FONTCONFIG_FILE = fontsConf;
  };
in
{ komposition = komposition;
  komposition-shell = shell;
}
