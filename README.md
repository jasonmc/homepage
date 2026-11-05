# Homepage generator

[![Website push](https://github.com/jasonmc/homepage/actions/workflows/main.yml/badge.svg)](https://github.com/jasonmc/homepage/actions/workflows/main.yml)

Build with `nix build`, or preview locally with `nix run .#watch`.

Merriweather, Roboto Slab, and Fira Code come from the pinned nixpkgs packages.
Nix prepares WOFF2 files in the Nix store and passes their directory to Hakyll via
`SITE_FONTS`. Hakyll copies them into `_site/fonts/` as part of its normal build.
The Nix build, preview command, and `nix develop` set this variable automatically.
