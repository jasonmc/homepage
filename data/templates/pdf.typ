#let ink = rgb("#2c2620")
#let muted = rgb("#6c6257")
#let accent = rgb("#a14d3a")
#let rule = rgb("#d6cabd")
#let code-bg = rgb("#f2e8da")
#let title = sys.inputs.at("title")
#let author = sys.inputs.at("author")
#let date = sys.inputs.at("date")

#set document(title: title, author: author, date: none)
#set page(
  paper: "a4",
  margin: (x: 30mm, top: 24mm, bottom: 26mm),
  numbering: "1",
  footer: context text(size: 9pt, fill: muted)[
    jasonmc.net #h(1fr) #counter(page).display("1")
  ],
)
#set text(font: "Source Serif 4", size: 11pt, fill: ink, lang: "en",
  number-type: "old-style", number-width: "proportional")
#set par(justify: true, leading: 0.65em, spacing: 0.8em)
#set heading(numbering: none)
#show heading: set text(weight: "semibold")
#show heading.where(level: 1): set text(size: 18pt)
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 12pt)
#show link: set text(fill: accent)
#show math.equation: set text(font: "STIX Two Math")
#set raw(theme: none)
#show raw: set text(font: "Fira Code", size: 0.85em,
  number-type: "lining", number-width: "tabular")
#show raw.where(block: true): it => block(
  width: 100%, fill: code-bg, inset: 10pt, breakable: true, it,
)
#set table(inset: 6pt, stroke: (top: none, right: none, bottom: 0.5pt + rule, left: none))
#set table.hline(stroke: 0.5pt + rule)
#show table: set text(number-type: "lining", number-width: "tabular")
#let horizontalrule = line(length: 100%, stroke: 0.5pt + rule)
#let pdf-diagram(svg, alt) = block(width: 100%, above: 1em, below: 1em)[
  #align(center, image(svg, format: "svg", width: 75%, alt: alt))
]

#block(below: 1.5em)[
  #text(font: "Source Serif 4 Display", size: 28pt, title)
  #v(0.5em)
  #text(size: 10pt, fill: muted)[
    #show "ᶜ": super[c]
    #author#if date != "" [ · #date]
  ]
]

$body$
