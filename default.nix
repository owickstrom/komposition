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

  giGtkDeclarativeJson = builtins.fromJSON (builtins.readFile ./gi-gtk-declarative.json);

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
      massiv = self.callHackage "massiv" "0.1.6.1" {};
      massiv-io = self.callHackage "massiv-io" "0.1.4.0" {};
      indexed-extras = pkgs.haskell.lib.doJailbreak super.indexed-extras;
      gi-gtk-declarative = self.callCabal2nix "gi-gtk-declarative" (nixpkgs.fetchFromGitHub {
        owner = "owickstrom";
        repo = "gi-gtk-declarative";
        inherit (giGtkDeclarativeJson) rev sha256;
      }) {};
    };
  };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callCabal2nix "fastcut" ./. {});
in
{ fastcut = drv;
  fastcut-shell = haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [drv];
  };
}
