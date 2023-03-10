{
  description = "The tao programming languagr";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = { self, nixpkgs, crane, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };

        rustTarget = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

        craneLib = (crane.mkLib pkgs).overrideToolchain rustTarget;

        tomlInfo = craneLib.crateNameFromCargoToml { cargoToml = ./compiler/Cargo.toml; };
        inherit (tomlInfo) version;
        pname = "tao";

        src =
          let
            markdownFilter = path: _type: pkgs.lib.hasSuffix ".md" path;
            filterPath = path: type: builtins.any (f: f path type) [
              markdownFilter
              craneLib.filterCargoSources
              pkgs.lib.cleanSourceFilter
            ];
          in
          pkgs.lib.cleanSourceWith {
            src = ./.;
            filter = filterPath;
          };

        cargoArtifacts = craneLib.buildDepsOnly {
          inherit src pname;

          buildInputs = [
            pkgs.pkg-config
          ];
        };

        tao = craneLib.buildPackage {
          inherit cargoArtifacts src pname version;

          cargoExtraArgs = "--all-features";

          buildInputs = [
            pkgs.pkg-config
          ];
        };
      in
      rec {
        checks = {
          inherit tao;

          tao-clippy = craneLib.cargoClippy {
            inherit cargoArtifacts src pname;
            cargoClippyExtraArgs = "--benches --examples --tests --all-features -- --deny warnings";
          };

          tao-fmt = craneLib.cargoFmt {
            inherit src pname;
          };
        };

        packages.tao = tao;
        packages.default = packages.tao;

        apps.tao = flake-utils.lib.mkApp {
          name = "tao";
          drv = tao;
        };
        apps.default = apps.tao;

        devShells.default = devShells.tao;
        devShells.tao = pkgs.mkShell {
          buildInputs = [
            pkgs.pkg-config
          ];

          nativeBuildInputs = [
            rustTarget

            pkgs.cargo-deny
            pkgs.gitlint
          ];
        };
      }
    );
}
