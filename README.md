# Homepage generator

[![Website push](https://github.com/jasonmc/homepage/actions/workflows/main.yml/badge.svg)](https://github.com/jasonmc/homepage/actions/workflows/main.yml)

Build with `nix build`, or preview locally with `nix run .#watch`.
The preview command finds the project root when run from a subdirectory.
Run `nix flake check` to build the site, validate its HTML with vnu, and check
its internal links.

Source Serif 4 and Fira Code come from the pinned nixpkgs font sources.
The 404 illustration comes from a pinned [IlluStatus](https://github.com/blairlee227/IlluStatus)
revision; its source credit and MIT license are included in the generated SVG.

Nix prepares these assets in the Nix store and passes their directory to Hakyll via
`SITE_ASSETS`. Hakyll copies them into `_site/` as part of its normal build.
The Nix build, preview command, and `nix develop` set this variable automatically.

## Diagrams in Org

Graphviz (`dot`, or `graphviz`) and Pikchr source blocks render to SVG images
embedded in the generated page. Nix supplies both renderers for builds, previews,
and the development shell.

```org
#+begin_src dot :alt "Source files become a website"
digraph {
  Source -> Website;
}
#+end_src

#+begin_src pikchr :alt "Two boxes joined by an arrow"
box "Source"; arrow; box "Website"
#+end_src
```

Use `:exports both` to show the source alongside the diagram, or `:exports code`
to keep a block as a code example. Omit `:exports results`: Pandoc hides the source
of those blocks before diagram rendering. Invalid diagrams fail the build.
