--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Context, Style, tri)
import Hakyll
import Hakyll.Core.Util.File (getRecursiveContents)
import Hakyll.Web.Pandoc
  ( defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
    pandocCompilerWithTransformM
  )
import System.Environment (getEnv, lookupEnv)
import Text.Pandoc.Highlighting (Style, styleToCss, tango)
import Text.Pandoc.Definition (Block (..), Inline (..))
import Text.Pandoc.Options (ReaderOptions, WriterOptions, writerHighlightStyle)
import Text.Pandoc.Walk (walkM)

domain :: String
domain = "jasonmc.net"

hakyllConfig :: Configuration
hakyllConfig =
  defaultConfiguration
    { providerDirectory = "data/"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "jasonmc.net latest posts",
      feedDescription = "Feed of Jason McCandless homepage posts.",
      feedAuthorName = "Jason McCandless",
      feedAuthorEmail = "me@jasonmc.net",
      feedRoot = "https://jasonmc.net"
    }

dragon :: Int -> Trail V2 Double
dragon 0 = fromOffsets [unitX]
dragon n = dragon (n - 1) <> rotateBy (1 / 4) (reverseTrail $ dragon (n - 1))

diagram :: Diagram B
diagram =
  strokeT (dragon 10)
    # lc (sRGB24read "#a14d3a")
    # lw medium
    # frame 1

diagramCompiler :: Compiler (Item String)
diagramCompiler = do
  let rendered = renderDia SVG (SVGOptions (mkWidth 250) Nothing "" [] True) diagram
  makeItem $ show rendered

highlightStyle :: Style
highlightStyle = tango

writerOptions :: WriterOptions
writerOptions =
  defaultHakyllWriterOptions
    { writerHighlightStyle = Just highlightStyle
    }

readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions

contentCompiler :: Compiler (Item String)
contentCompiler =
  pandocCompilerWithTransformM readerOptions writerOptions (walkM renderCodeDiagram)

renderCodeDiagram :: Block -> Compiler Block
renderCodeDiagram block@(CodeBlock (identifier, classes, attributes) source)
  | not (any (`elem` classes) ["dot", "graphviz", "pikchr"]) = pure block
  | lookup "exports" attributes == Just "code" = pure code
  | "dot" `elem` classes || "graphviz" `elem` classes =
      render "dot" ["-Tsvg", "-Gbgcolor=transparent"] "Graphviz diagram"
  | "pikchr" `elem` classes = render "pikchr" ["--svg-only", "-"] "Pikchr diagram"
  where
    code = CodeBlock (identifier, classes, filter ((/= "alt") . fst) attributes) source
    render program arguments fallbackAlt = do
      svg <- unixFilterLBS program arguments (BL.fromStrict (TE.encodeUtf8 source))
      stylesheet <- loadBody "css/diagrams.css"
      let diagramStyle = "<style>" <> T.pack stylesheet <> "</style>"
          styledSvg = T.replace "</svg>" (diagramStyle <> "</svg>") (TE.decodeUtf8 (BL.toStrict svg))
          uri = "data:image/svg+xml;base64," <> TE.decodeUtf8 (Base64.encode (TE.encodeUtf8 styledSvg))
          alt = T.dropAround (== '"') (fromMaybe fallbackAlt (lookup "alt" attributes))
          showSource = lookup "exports" attributes == Just "both"
          imageId = if showSource then "" else identifier
          image = Para [Image (imageId, ["diagram"], []) [Str alt] (uri, "")]
      pure $ if showSource
        then Div ("", [], []) [code, image]
        else image
renderCodeDiagram block = pure block

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConfig $ do
  (gitRevision, assetDirectory, assets) <- preprocess $ do
    gitRevision <- fmap (fromMaybe "unknown") (lookupEnv "GIT_COMMIT")
    assetDirectory <- getEnv "SITE_ASSETS"
    assetFiles <- getRecursiveContents (const $ pure False) assetDirectory
    assets <- forM assetFiles $ \asset -> do
      bytes <- BS.readFile (assetDirectory ++ "/" ++ asset)
      pure (asset, bytes)
    pure (gitRevision, assetDirectory, assets)
  siteRules gitRevision assetDirectory assets

siteRules :: String -> FilePath -> [(FilePath, BS.ByteString)] -> Rules ()
siteRules gitRevision assetDirectory assets = do
  let repository = "https://github.com/jasonmc/homepage"
      revisionUrl =
        if gitRevision == "unknown"
          then repository
          else repository ++ "/commit/" ++ takeWhile (/= '-') gitRevision
      baseCtx =
        constField "gitRevision" gitRevision
          `mappend` constField "gitRevisionUrl" revisionUrl
          `mappend` defaultContext
      postCtx = dateField "date" "%B %e, %Y" `mappend` baseCtx

  -- The immutable store path invalidates cached assets when their inputs change.
  version assetDirectory $
    forM_ assets $ \(asset, bytes) ->
      create [fromFilePath asset] $ do
        route idRoute
        compile $ makeItem bytes

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "favicon/*" $ do
    route idRoute
    compile copyFileCompiler

  match ("css/*" .&&. complement "css/diagrams.css") $ do
    route idRoute
    compile compressCssCompiler

  -- Compile the self-contained SVG stylesheet once, with tracked dependencies.
  match "css/diagrams.css" $ compile $ do
    font <- loadBody $ setVersion (Just assetDirectory) "fonts/source-serif-4.woff2"
    let fontUri = "data:font/woff2;base64," <> TE.decodeUtf8 (Base64.encode font)
    stylesheet <- compressCssCompiler
    pure $ fmap (T.unpack . T.replace "/fonts/source-serif-4.woff2" fontUri . T.pack) stylesheet

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ makeItem (styleToCss highlightStyle)

  create ["diagram.svg"] $ do
    route idRoute
    compile diagramCompiler

  match (fromList ["about.rst", "contact.markdown", "colophon.org"]) $ do
    route $ setExtension "html"
    compile $
      contentCompiler
        >>= loadAndApplyTemplate "templates/default.html" baseCtx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      contentCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <-
        fmap (take 10) . recentFirst
          =<< loadAllSnapshots "posts/*" "content"
      renderRss feedConfiguration feedCtx posts

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` baseCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` baseCtx

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "404.html" $ do
    route idRoute
    compile $ do
      getResourceBody
        >>= applyAsTemplate baseCtx
        >>= loadAndApplyTemplate "templates/default.html" baseCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem domain
