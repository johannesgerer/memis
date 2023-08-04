# -*- compile-command: "nix-shell --run 'cabal exec -- ghc-pkg list'"; -*-
{ sources ? import ./nix/sources.nix {}, pkgs ? null }:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix
let pkgs2 = if pkgs != null then pkgs else import sources.nixpkgs { overlays = [cabalHashes]; }; 
    cabalHashes = self: super: { inherit (sources) all-cabal-hashes; };
in
let pkgs = pkgs2;
in pkgs.haskellPackages.developPackage {
    root = ./.;
    withHoogle = false;
    returnShellEnv = false;
    modifier = with pkgs.haskell.lib; drv:
      disableLibraryProfiling (dontHaddock (addBuildTools drv
        (with pkgs.haskellPackages; [ cabal-install ghcid])));
    overrides = self: super: {
      simple-templates = self.callHackage "simple-templates" "2.0.0" {};
      simple = self.callHackage "simple" "2.0.0" {};
      # simple-templates = self.callHackageDirect {
      #   pkg ="simple-templates";
      #   ver = "2.0.0";
      #   sha256 = "sha256-qPpBwzESQuzRgP6oacjSc6GK3RJzfeOZlD7dtCjx8P4=";
      # } {};
      # simple = self.callHackageDirect {
      #   pkg ="simple";
      #   ver = "2.0.0";
      #   sha256 = "sha256-qPpBwzESQuzRgP6oacjSc6GK3RJzfeOZlD7dtCjx8P4=";
      # } {};
      # simple-templates = self.callCabal2nix "simple-templates" ../simple/simple-templates {};
      # simple = self.callCabal2nix "simple" ../simple/simple {};
    };
  }
