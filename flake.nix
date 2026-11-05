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

        fonts = pkgs.runCommand "website-fonts" { } ''
          mkdir -p "$out"
          cp ${pkgs.source-serif.src}/WOFF2/VAR/SourceSerif4Variable-Roman.ttf.woff2 "$out/source-serif-4.woff2"
          cp ${pkgs.source-serif.src}/WOFF2/VAR/SourceSerif4Variable-Italic.ttf.woff2 "$out/source-serif-4-italic.woff2"
          cp ${pkgs.source-serif.src}/LICENSE.md "$out/source-serif-4-LICENSE.md"
          cp ${pkgs.fira-code.src}/woff2/FiraCode-Regular.woff2 "$out/fira-code.woff2"
        '';

        illustatus = pkgs.fetchFromGitHub {
          owner = "blairlee227";
          repo = "IlluStatus";
          rev = "60822636928e525b758e1e0c1893cd5a8ae5bff3";
          hash = "sha256-FZDbERMih1Ojym6daIjeLXIC2ZM5aEl1y/NBz3Axx60=";
        };

        assets = pkgs.runCommand "website-assets" { } ''
          mkdir -p "$out/fonts" "$out/images"
          cp ${fonts}/* "$out/fonts/"
          {
            printf '<!--\nSource: https://github.com/blairlee227/IlluStatus\n\n'
            cat ${illustatus}/LICENSE
            printf '\n-->\n'
            sed -n '/<svg id="mainImage_create"/,/<\/svg>/p' ${illustatus}/index.html \
              | sed '/@import.*fonts\.googleapis\.com/d; s/#4f86ed/#b5041b/g'
          } > "$out/images/404.svg"
        '';

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
            (pkgs.stdenv.buildPlatform.libc == "glibc")
            "${pkgs.glibcLocales}/lib/locale/locale-archive";

          GIT_COMMIT = gitRev;
          SITE_ASSETS = assets;

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

        watch = pkgs.writeShellApplication {
          name = "watch";
          runtimeInputs = [ builder ];
          text = ''
            export SITE_ASSETS=${assets}
            export GIT_COMMIT=${pkgs.lib.escapeShellArg gitRev}
            exec site watch "$@"
          '';
        };
      in
      {
        packages.builder = builder;
        packages.fonts = fonts;
        packages.assets = assets;
        packages.website = site;
        packages.website-pages = pagesArtifact;

        packages.default = self.packages.${system}.website;

        checks.internal-links = pkgs.testers.lycheeLinkCheck {
          inherit site;
          relocatable = false;
        };

        checks.html = pkgs.runCommand "website-html-check"
          {
            nativeBuildInputs = [ pkgs.validator-nu ];
            inherit site;
          }
          ''
            vnu --skip-non-html "$site"
            touch "$out"
          '';

        apps.watch = { type = "app"; program = "${watch}/bin/watch"; };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ builder ];
          GIT_COMMIT = gitRev;
          SITE_ASSETS = assets;
          buildInputs = with haskellPackages; site.buildInputs ++ [
            haskell-language-server
            cabal-install
          ];
        };
      });
}
