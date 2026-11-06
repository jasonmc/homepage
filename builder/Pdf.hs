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
    "#block(above: 1em, below: 1em)[#image(" <> typstBytes (withFonts svg)
      <> ", format: \"svg\", alt: str(" <> typstBytes (fromMaybe "Diagram" (lookup "aria-label" attributes)) <> "))]"
  where
    withFonts = T.pack . withTags font . T.unpack
    font (TagOpen "svg" attrs) =
      TagOpen "svg" (("xmlns", "http://www.w3.org/2000/svg") : filter ((/= "xmlns") . fst) attrs)
    font (TagOpen "text" attrs) =
      TagOpen "text" (("font-family", "Source Serif 4") : filter ((/= "font-family") . fst) attrs)
    font tag = tag
pdfDiagram block = block

typstBytes :: T.Text -> T.Text
typstBytes text = "bytes((" <> T.intercalate "," (map (T.pack . show) (BS.unpack (TE.encodeUtf8 text))) <> "))"
