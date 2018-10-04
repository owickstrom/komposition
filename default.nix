{ compiler ? "ghc843", doBenchmark ? false, doProfiling ? false }:

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
  toggleBenchmark = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  toggleLibraryProfiling = if doBenchmark then pkgs.haskell.lib.enableLibraryProfiling else pkgs.haskell.lib.disableLibraryProfiling;
  toggleExecutableProfiling = if doBenchmark then pkgs.haskell.lib.enableExecutableProfiling else pkgs.haskell.lib.disableExecutableProfiling;

  drv = toggleExecutableProfiling
        (toggleLibraryProfiling
         (toggleBenchmark
          (pkgs.haskell.lib.dontHaddock (haskellPackages.callCabal2nix "komposition" ./. {}))));
  mkdocs = import ./mkdocs.nix { inherit nixpkgs; };
in
{ komposition = drv;
  komposition-shell = haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [drv];
    buildInputs = with pkgs; [ cabal-install mkdocs.packages  ];
  };
}
