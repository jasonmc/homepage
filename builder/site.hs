--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Context, Style, tri)
import Hakyll
import Hakyll.Web.Pandoc
  ( defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
    pandocCompilerWith
  )
import System.Directory (listDirectory)
import System.Environment (getEnv, lookupEnv)
import Text.Pandoc.Highlighting (Style, styleToCss, tango)
import Text.Pandoc.Options (ReaderOptions, WriterOptions, writerHighlightStyle)

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
    # lc (sRGB24read "#a50104")
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

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConfig $ do
  (gitRevision, fontDirectory, fonts) <- preprocess $ do
    gitRevision <- fmap (fromMaybe "unknown") (lookupEnv "GIT_COMMIT")
    fontDirectory <- getEnv "SITE_FONTS"
    fontFiles <- listDirectory fontDirectory
    fonts <- forM fontFiles $ \font -> do
      bytes <- BS.readFile (fontDirectory ++ "/" ++ font)
      pure (font, bytes)
    pure (gitRevision, fontDirectory, fonts)
  siteRules gitRevision fontDirectory fonts

siteRules :: String -> FilePath -> [(FilePath, BS.ByteString)] -> Rules ()
siteRules gitRevision fontDirectory fonts = do
  let baseCtx = constField "gitRevision" gitRevision `mappend` defaultContext
      postCtx = dateField "date" "%B %e, %Y" `mappend` baseCtx

  -- The immutable store path invalidates cached fonts when their package changes.
  version fontDirectory $
    forM_ fonts $ \(font, bytes) ->
      create [fromFilePath ("fonts/" ++ font)] $ do
        route idRoute
        compile $ makeItem bytes

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "favicon/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ makeItem (styleToCss highlightStyle)

  create ["diagram.svg"] $ do
    route idRoute
    compile diagramCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompilerWith readerOptions writerOptions
        >>= loadAndApplyTemplate "templates/default.html" baseCtx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompilerWith readerOptions writerOptions
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
