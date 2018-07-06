{ compiler ? "ghc843", doBenchmark ? false }:

let
  bootstrap = import <nixpkgs> { };
  nixpkgsJson = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  nixpkgsSrc = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgsJson) rev sha256;
  };
  nixpkgs = import nixpkgsSrc {};

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
      massiv = self.callHackage "massiv" "0.1.6.1" {};
      massiv-io = self.callHackage "massiv-io" "0.1.4.0" {};
      gi-gtk-declarative = self.callPackage ./lib/gi-gtk-declarative/gi-gtk-declarative.nix {};
      indexed-extras = pkgs.haskell.lib.doJailbreak super.indexed-extras;
    };
  };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callPackage (import ./fastcut.nix) {});
in
  if pkgs.lib.inNixShell then drv.env else drv
