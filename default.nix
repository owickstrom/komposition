{ compiler ? "ghc862", doCheck ? true, doBenchmark ? false }:

let
  nixpkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2018-12-09";
    url = https://github.com/nixos/nixpkgs/;
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    rev = "e85c1f586807b5acd244df4c45a5130aa3f0734d";
  }) {};

  fusedEffectsJson = builtins.fromJSON (builtins.readFile ./fused-effects.json);

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      criterion = self.callHackage "criterion" "1.5.3.0" {};
      ffmpeg-light = pkgs.haskell.lib.doJailbreak super.ffmpeg-light;
      fused-effects =
        let
          src = nixpkgs.fetchFromGitHub {
            owner = "robrix";
            repo = "fused-effects";
            inherit (fusedEffectsJson) rev sha256;
          };
        in self.callCabal2nix "fused-effects" src {};
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
      indexed-extras = pkgs.haskell.lib.doJailbreak super.indexed-extras;
      massiv = self.callHackage "massiv" "0.2.5.0" {};
      massiv-io = self.callHackage "massiv-io" "0.1.4.0" {};
      pipes-safe = self.callHackage "pipes-safe" "2.3.1" {};
      protolude = self.callHackage "protolude" "0.2.3" {};
      gi-gtk-declarative = self.callHackage "gi-gtk-declarative" "0.4.1" {};
    };
  };
  toggleCheck = if doCheck then pkgs.haskell.lib.doCheck else pkgs.haskell.lib.dontCheck;
  toggleBenchmark = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = toggleBenchmark
        (toggleCheck
         (pkgs.haskell.lib.justStaticExecutables
          (pkgs.haskell.lib.dontHaddock (haskellPackages.callCabal2nix "komposition" ./. {}))));

  python = import ./docs/requirements.nix { pkgs = nixpkgs; };

  komposition = nixpkgs.stdenv.mkDerivation {
    name = "komposition";
    nativeBuildInputs = with nixpkgs; [ wrapGAppsHook makeWrapper ];
    buildInputs = with nixpkgs; [
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
    src = ./.;
    buildPhase = ''
        mkdir -p $out
      '';
    installPhase = ''
      mkdir -p $out/bin
      ln -s ${drv}/bin/komposition $out/bin
      wrapProgram $out/bin/komposition \
        --prefix 'PATH' ':' "${pkgs.ffmpeg}/bin" \
        --prefix 'PATH' ':' "${pkgs.sox}/bin"
    '';
  };
in
{ komposition = komposition;
  komposition-shell = haskellPackages.shellFor {
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
  };
}
