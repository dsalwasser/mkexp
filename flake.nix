{
  description = "Scripts to install and run experiments with various graph partitioners.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };

      inputs = [
        pkgs.gettext
        pkgs.R
        pkgs.rPackages.ggplot2
        pkgs.rPackages.plyr
        pkgs.rPackages.dplyr
        pkgs.rPackages.RColorBrewer
        pkgs.rPackages.gridExtra
        pkgs.rPackages.egg
      ];

      devInputs = [
        pkgs.fish
        pkgs.rPackages.languageserver
      ];

      mkexp = pkgs.stdenvNoCC.mkDerivation {
        pname = "mkexp";
        version = "1.0.0";

        src = self;
        buildInputs = inputs;

        dontBuild = true;

        installPhase = ''
          cp -r $src $out
        '';

        meta = {
          description = "Scripts to install and run experiments with various graph partitioners.";
          homepage = "https://github.com/DanielSeemaier/mkexp";
        };
      };

      mkDevShell = additionalInputs: pkgs.mkShell {
        packages = inputs ++ devInputs ++ additionalInputs;

        shellHook = ''
          exec fish
        '';
      };
    in
    {
      devShells = {
        default = mkDevShell [ ];

        mkexp = mkDevShell [ mkexp ];

        kaminpar = mkDevShell ([ mkexp ] ++ devInputs ++ builtins.attrValues {
          inherit (pkgs) cmake ninja python312 gcc13 tbb_2021_8 sparsehash mpi;
        });
      };

      packages.default = mkexp;
    }
  );
}
