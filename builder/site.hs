--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Context, tri)
import Hakyll
import System.Environment (lookupEnv)

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

gitContext :: Context String
gitContext =
  field "gitRevision" $ \_ ->
    unsafeCompiler $
      fmap (fromMaybe "unknown") (lookupEnv "GIT_COMMIT")

baseCtx :: Context String
baseCtx = gitContext `mappend` defaultContext

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConfig $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "favicon/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "fonts/*" $ do
    route idRoute
    compile copyFileCompiler

  create ["diagram.svg"] $ do
    route idRoute
    compile diagramCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" baseCtx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
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

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` baseCtx
