# You can build this repository using Nix by running:
#
#     $ nix-build release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell
#
# ... and then Nix will supply the correct Haskell development environment for
# you
{ pkgs ? import <nixpkgs> {} }:
let
  name = "upcast";

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          "${name}" = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  custom-pkgs =
    import pkgs.path { inherit config; inherit (pkgs) system; };

in
  { "${name}" = custom-pkgs.haskellPackages."${name}";
  }
