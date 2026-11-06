# Homepage generator

[![Website push](https://github.com/jasonmc/homepage/actions/workflows/main.yml/badge.svg)](https://github.com/jasonmc/homepage/actions/workflows/main.yml)

Build with `nix build`, or preview locally with `nix run .#watch`.
The preview command finds the project root when run from a subdirectory.
Run `nix flake check` to build the site, validate its HTML with vnu, and check
its internal links.

Source Serif 4 and Fira Code come from the pinned nixpkgs font sources.

Nix prepares the web fonts directly in a `fonts/` directory in the Nix store and
passes that output to Hakyll via
`SITE_ASSETS`. Hakyll copies them into `_site/` as part of its normal build.
The Nix build, preview command, and `nix develop` set this variable automatically.

Hakyll generates the italic `jmc.` favicon with `diagrams`, using Source Serif 4 and the
site's colors. Nix supplies the font and SVG renderer. The SVG has outlined
lettering, and Hakyll derives the PNG sizes from it.

The homepage dragon curve and the broken infinity curve on the 404 page are
also generated with `diagrams` in `builder/site.hs`.

`data/css/palette.css` defines the light and dark palettes. The page and generated
SVG artwork share it, following the browser's color preference. Inline Pikchr and
Graphviz diagrams use the same palette through `diagrams.css`. PNG icons retain
the light palette, and PDF editions use their print palette.

## Section links

Content headings have a `§` permalink, such as `/colophon.html#nix`.
To keep an Org section's link stable when renaming its heading, set `CUSTOM_ID`:

```org
** Nix
:PROPERTIES:
:CUSTOM_ID: nix
:END:
```

## Diagrams in Org

Graphviz (`dot`, or `graphviz`) and Pikchr source blocks render to inline SVG
in the generated page. Nix supplies both renderers for builds, previews,
and the development shell.

Diagrams share the page's fonts and stylesheets. `data/css/diagrams.css` defines
their typography and applies the site's color palette to Pikchr diagrams.
Use `fill white` in Pikchr for shapes that should receive the accent fill and outline.

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

## PDF editions

The colophon and posts also build as PDFs at the same path with a `.pdf`
extension, such as `/colophon.pdf`. Their HTML pages include a PDF download
link. PDF editions use the same content sources; `data/templates/pdf.typ`
defines the print layout, headings, code blocks, and page footers.

Pandoc writes Typst, and Typst compiles the PDF during the Hakyll build.
Nix supplies Typst and pins Source Serif 4, Fira Code, and STIX Two Math.
System and embedded fallback fonts are disabled. The PDF embeds the fonts it
uses, and Graphviz and Pikchr diagrams remain vector graphics.

Post dates come from their metadata or dated filename. Relative links point
back to the corresponding website URLs. PDF versions stay out of the HTML
archive and RSS feed. Builds, previews, and the development shell all use
the same PDF toolchain; generated files stay in `_site/` or the Nix store.
