{-# LANGUAGE OverloadedStrings #-}

module Pdf (pdfCompiler, pdfField) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hakyll
import Network.URI (parseURI, parseURIReference, relativeTo)
import Text.HTML.TagSoup (Tag (..))
import qualified Text.Pandoc as P
import Text.Pandoc.Walk (walk)

pdfCompiler :: String -> (P.Pandoc -> Compiler P.Pandoc) -> Compiler (Item BL.ByteString)
pdfCompiler domain renderDiagrams = do
  source <- getResourceBody
  parsed <- readPandocWith defaultHakyllReaderOptions source
  metadata <- getMetadata (itemIdentifier source)
  route <- getRoute (setVersion Nothing (itemIdentifier source))
  date <- if matches "posts/*" (setVersion Nothing (itemIdentifier source))
    then Just . formatTime defaultTimeLocale "%B %-d, %Y"
      <$> getItemUTC defaultTimeLocale (itemIdentifier source)
    else pure (lookupString "date" metadata)
  document <- renderDiagrams (itemBody parsed)
  let title = fromMaybe "" (lookupString "title" metadata)
      author = fromMaybe "Jason MᶜCandless" (lookupString "author" metadata)
      base = parseURI ("https://" ++ domain ++ maybe "/" toUrl route)
      absoluteLink (P.Link attrs label (url, title))
        | not ("#" `T.isPrefixOf` url)
        , Just origin <- base
        , Just target <- parseURIReference (T.unpack url) =
            P.Link attrs label (T.pack (show (target `relativeTo` origin)), title)
      absoluteLink inline = inline
  body <- either (fail . show) pure $ P.runPure $
    P.writeTypst P.def (walk absoluteLink (walk pdfDiagram document))
  typst <- makeItem (T.unpack body)
    >>= loadAndApplyTemplate "templates/pdf.typ" defaultContext
  pdf <- unixFilterLBS "typst"
    ["compile", "--format", "pdf", "--input", "title=" ++ title,
     "--input", "author=" ++ author, "--input", "date=" ++ fromMaybe "" date, "-", "-"]
    (BL.fromStrict (TE.encodeUtf8 (T.pack (itemBody typst))))
  makeItem pdf

pdfField :: Context String
pdfField = field "pdfUrl" $ \item -> do
  route <- getRoute (setVersion (Just "pdf") (itemIdentifier item))
  maybe (noResult "No PDF edition") (pure . toUrl) route

pdfDiagram :: P.Block -> P.Block
pdfDiagram (P.Div (_, ["diagram"], attributes) [P.RawBlock (P.Format "html") svg]) =
  P.RawBlock (P.Format "typst") $
    "#pdf-diagram(" <> typstBytes (prepareSvg svg)
      <> ", str(" <> typstBytes (fromMaybe "Diagram" (lookup "aria-label" attributes)) <> "))"
  where
    prepareSvg = T.replace "\xA0" " " . T.pack . withTags font . T.unpack
    pikchr = "class=\"pikchr\"" `T.isInfixOf` svg
    style properties attrs =
      ("style", fromMaybe "" (lookup "style" attrs) ++ ";" ++ properties)
        : filter ((/= "style") . fst) attrs
    font (TagOpen "svg" attrs) =
      TagOpen "svg" (("xmlns", "http://www.w3.org/2000/svg") : filter ((/= "xmlns") . fst) attrs)
    font (TagOpen "text" attrs) =
      TagOpen "text" $ ("font-family", "Source Serif 4")
        : filter ((/= "font-family") . fst) (if pikchr then style "fill:#2c2620" attrs else attrs)
    font (TagOpen "path" attrs) | pikchr =
      TagOpen "path" $ style
        (if "fill:rgb(" `T.isInfixOf` T.pack (fromMaybe "" (lookup "style" attrs))
          then "fill:#f2e8da;stroke:#a14d3a" else "stroke:#6c6257") attrs
    font (TagOpen "polygon" attrs) | pikchr =
      TagOpen "polygon" (style "fill:#6c6257" attrs)
    font (TagText text) = TagText (map (\c -> if c == '\xA0' then ' ' else c) text)
    font tag = tag
pdfDiagram (P.Header level attrs inlines) = P.Header (max 1 (level - 1)) attrs inlines
pdfDiagram block = block

typstBytes :: T.Text -> T.Text
typstBytes text = "bytes((" <> T.intercalate "," (map (T.pack . show) (BS.unpack (TE.encodeUtf8 text))) <> "))"
