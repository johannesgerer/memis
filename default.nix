# -*- compile-command: "nix-shell --run 'cabal exec -- ghc-pkg list'"; -*-
let cabalHashes = self: super: {
      all-cabal-hashes = super.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/ef065b55b68cd8a66da14319420f1d01bb849d96.tar.gz";
        sha256 = "sha256-cxUbv6/NyCBdbinTS9R6QupgvMuNtui7eFSsWyAX0qQ=";
      };};
in { pkgs ? import (import ./nix/sources.nix {}).nixpkgs { overlays = [cabalHashes]; },
     sources ? import ./nix/sources.nix {} }:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix
pkgs.haskellPackages.developPackage {
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
