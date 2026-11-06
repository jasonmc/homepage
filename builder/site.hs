--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM, forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import qualified Data.ByteString as BS
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
import Text.HTML.TagSoup (Tag (..))
import Text.Pandoc.Highlighting (Style, monochrome, styleToCss)
import Text.Pandoc.Definition (Block (..), Format (..), Inline (..))
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

favicon :: Diagram B
favicon =
  (text "jmc." # font "Source Serif 4" # italic # fontSizeL 0.40 # fc (sRGB24read "#f8f5f0"))
    <> (square 1 # fc (sRGB24read "#a14d3a") # lw none)

faviconCompiler :: Compiler (Item BL.ByteString)
faviconCompiler = do
  let svg = show $ renderDia SVG (SVGOptions (mkWidth 512) Nothing "" [] True) favicon
  outlined <- unixFilterLBS "rsvg-convert" ["--format", "svg"] (BL.fromStrict (TE.encodeUtf8 (T.pack svg)))
  makeItem outlined

highlightStyle :: Style
highlightStyle = monochrome

writerOptions :: WriterOptions
writerOptions =
  defaultHakyllWriterOptions
    { writerHighlightStyle = Just highlightStyle
    }

readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions

contentCompiler :: Compiler (Item String)
contentCompiler =
  pandocCompilerWithTransformM readerOptions writerOptions $
    \document -> evalStateT (walkM (renderCodeDiagram . addHeadingAnchor) document) 1

addHeadingAnchor :: Block -> Block
addHeadingAnchor (Header level attributes@(identifier, _, _) inlines)
  | not (T.null identifier) =
      Header level attributes (inlines ++ [Space, permalink])
  where
    permalink = Link ("", ["heading-anchor"], [("aria-label", "Link to this section")])
      [Str "§"] ("#" <> identifier, "")
addHeadingAnchor block = block

renderCodeDiagram :: Block -> StateT Int Compiler Block
renderCodeDiagram block@(CodeBlock (identifier, classes, attributes) source)
  | not (any (`elem` classes) ["dot", "graphviz", "pikchr"]) = pure block
  | lookup "exports" attributes == Just "code" = pure code
  | otherwise = do
      number <- state (\n -> (n, n + 1))
      let (program, arguments, fallbackAlt) =
            if "dot" `elem` classes || "graphviz" `elem` classes
              then ("dot", ["-Tsvg_inline", "-Gbgcolor=transparent", "-Gid=code-diagram-" ++ show number], "Graphviz diagram")
              else ("pikchr", ["--svg-only", "-"], "Pikchr diagram")
      svg <- lift $ unixFilterLBS program arguments (BL.fromStrict (TE.encodeUtf8 source))
      let alt = T.dropAround (== '"') (fromMaybe fallbackAlt (lookup "alt" attributes))
          showSource = lookup "exports" attributes == Just "both"
          imageId = if showSource then "" else identifier
          image = Div (imageId, ["diagram"], [("role", "img"), ("aria-label", alt)])
            [RawBlock (Format "html") (TE.decodeUtf8 (BL.toStrict svg))]
      pure $ if showSource
        then Div ("", [], []) [code, image]
        else image
  where
    code = CodeBlock (identifier, classes, filter ((/= "alt") . fst) attributes) source
renderCodeDiagram block = pure block

navigationField :: Context String
navigationField = field "navigation" $ \item -> do
  route <- getRoute (itemIdentifier item)
  links <- loadAndApplyTemplate "templates/navigation.html" defaultContext item
  let url = if route == Just "index.html" then "/" else maybe "" toUrl route
      mark (TagOpen "a" attributes)
        | lookup "href" attributes == Just url =
          TagOpen "a" (("aria-current", "page") : attributes)
      mark tag = tag
  pure $ withTags mark (itemBody links)

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
        navigationField
          `mappend` constField "gitRevision" gitRevision
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

  version assetDirectory $ do
    create ["favicon/favicon.svg"] $ do
      route idRoute
      compile faviconCompiler

    forM_ [(16, "favicon-16x16.png"), (32, "favicon-32x32.png"),
           (180, "apple-touch-icon.png"), (192, "android-chrome-192x192.png"), (512, "android-chrome-512x512.png")] $ \(size, name) ->
      create [fromFilePath ("favicon/" ++ name)] $ do
        route idRoute
        compile $ do
          svg <- loadBody $ setVersion (Just assetDirectory) "favicon/favicon.svg"
          png <- unixFilterLBS "rsvg-convert" ["--width", show (size :: Int), "--height", show size] svg
          makeItem png

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

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
            boolField "hasPosts" (const $ not $ null posts)
              `mappend` listField "posts" postCtx (return posts)
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

  match "templates/*" $ compile templateBodyCompiler

  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem domain
