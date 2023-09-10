{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        builder = haskellPackages.callCabal2nix "Builder" "${./builder}" { };

        site = pkgs.stdenv.mkDerivation {
          name = "website";
          buildInputs = [ ];
          src = pkgs.nix-gitignore.gitignoreSourcePure [
            ./.gitignore
            ".git"
            ".github"
          ] ./data;

          # LANG and LOCALE_ARCHIVE are fixes pulled from the community:
          #   https://github.com/jaspervdj/hakyll/issues/614#issuecomment-411520691
          #   https://github.com/NixOS/nix/issues/318#issuecomment-52986702
          #   https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE = pkgs.lib.optionalString
            (pkgs.buildPlatform.libc == "glibc")
            "${pkgs.glibcLocales}/lib/locale/locale-archive";

          buildPhase = ''
            ${builder}/bin/site build --verbose
          '';

          installPhase = ''
            mkdir -p "$out/dist"
            cp -a _site/. "$out/dist"
          '';
        };
      in
      {
        packages.builder = builder;
        packages.website = site;

        packages.default = self.packages.${system}.website;


        devShell = haskellPackages.shellFor {
          packages = p: [ builder ];
          buildInputs = with haskellPackages; site.buildInputs ++ [
            haskell-language-server
            cabal-install
          ];
        };
      });
}
