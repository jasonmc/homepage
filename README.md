# Homepage generator

[![Website push](https://github.com/jasonmc/homepage/actions/workflows/main.yml/badge.svg)](https://github.com/jasonmc/homepage/actions/workflows/main.yml)

Build with `nix build`, or preview locally with `nix run .#watch`.
Run `nix flake check` to build the site, validate its HTML with vnu, and check
its internal links.

Merriweather, Roboto Slab, and Fira Code come from the pinned nixpkgs packages.
The 404 illustration comes from a pinned [IlluStatus](https://github.com/blairlee227/IlluStatus)
revision; its source credit and MIT license are included in the generated SVG.

Nix prepares these assets in the Nix store and passes their directory to Hakyll via
`SITE_ASSETS`. Hakyll copies them into `_site/` as part of its normal build.
The Nix build, preview command, and `nix develop` set this variable automatically.
