{ compiler ? "ghc843", doCheck ? true, doBenchmark ? false }:

let
  nixpkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2018-10-02";
    url = https://github.com/nixos/nixpkgs/;
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    rev = "46651b82b87318e37440c15a639d49ec05e79b79";
  }) {};

  giGtkDeclarativeJson = builtins.fromJSON (builtins.readFile ./gi-gtk-declarative.json);

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
      massiv = self.callHackage "massiv" "0.1.6.1" {};
      massiv-io = self.callHackage "massiv-io" "0.1.4.0" {};
      indexed-extras = pkgs.haskell.lib.doJailbreak super.indexed-extras;
      gi-gtk-declarative =
        let
          src = nixpkgs.fetchFromGitHub {
            owner = "owickstrom";
            repo = "gi-gtk-declarative";
            inherit (giGtkDeclarativeJson) rev sha256;
          };
        in self.callCabal2nix "gi-gtk-declarative" "${src}/gi-gtk-declarative" {};
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
