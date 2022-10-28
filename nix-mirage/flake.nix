{
  description = "Hello";

  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.inputs.flake-utils.follows = "flake-utils";
  inputs.nixpkgs.url = "./nix-overlays";

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_4_14;
        });
      in
      rec {
        packages = {
          native = pkgs.callPackage ./nix {
            nix-filter = nix-filter.lib;
            doCheck = false;
          };
          musl64 =
            let
              pkgs' = pkgs.pkgsCross.musl64;
            in
            pkgs'.lib.callPackageWith pkgs' ./nix {
              static = true;
              doCheck = false;
              nix-filter = nix-filter.lib;
            };
          solo5 =
            let
              pkgs' = pkgs.pkgsCross.solo5;
            in
            pkgs'.lib.callPackageWith pkgs' ./nix {
              doCheck = false;
              nix-filter = nix-filter.lib;
            };
        };
        defaultPackage = packages.native.hello-spt;
        devShell = pkgs.callPackage ./shell.nix { inherit packages; };
      });
}