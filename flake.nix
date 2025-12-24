{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        sourceInfo = self.sourceInfo or { };
        gitRev =
          if sourceInfo ? dirtyShortRev then sourceInfo.dirtyShortRev
          else if sourceInfo ? shortRev then sourceInfo.shortRev
          else if sourceInfo ? rev then builtins.substring 0 7 sourceInfo.rev
          else "unknown";

        builder = haskellPackages.callCabal2nix "Builder" "${./builder}" { };

        site = pkgs.stdenv.mkDerivation {
          name = "website";
          buildInputs = [ ];
          src = pkgs.nix-gitignore.gitignoreSourcePure [
            ./.gitignore
            ".git"
            ".github"
          ] ./.;

          # LANG and LOCALE_ARCHIVE are fixes pulled from the community:
          #   https://github.com/jaspervdj/hakyll/issues/614#issuecomment-411520691
          #   https://github.com/NixOS/nix/issues/318#issuecomment-52986702
          #   https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE = pkgs.lib.optionalString
            (pkgs.buildPlatform.libc == "glibc")
            "${pkgs.glibcLocales}/lib/locale/locale-archive";

          GIT_COMMIT = gitRev;

          buildPhase = ''
            ${builder}/bin/site build --verbose
          '';

          installPhase = ''
            cp -a _site/. "$out"
          '';
        };

        pagesArtifact = pkgs.runCommand "website-pages.tar"
          { buildInputs = [ pkgs.gnutar ]; }
          ''
            tar \
              --dereference \
              --hard-dereference \
              --owner=root \
              --group=root \
              --directory ${site} \
              -cf "$out" \
              .
          '';

        watch = pkgs.runCommand "watch"
          {
            buildInputs = [ pkgs.makeWrapper ];
          }
          ''
            makeWrapper "${pkgs.writeScript "watch-unwrapped" "site watch"}" "$out" ${builtins.toString  (map (p: "--prefix PATH : ${p}/bin") [ builder ])}
          '';
      in
      {
        packages.builder = builder;
        packages.website = site;
        packages.website-pages = pagesArtifact;

        packages.default = self.packages.${system}.website;

        apps.watch = { type = "app"; program = "${watch}"; };

        devShell = haskellPackages.shellFor {
          packages = p: [ builder ];
          buildInputs = with haskellPackages; site.buildInputs ++ [
            haskell-language-server
            cabal-install
          ];
        };
      });
}
