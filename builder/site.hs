--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Diagrams.Prelude hiding (tri, Context)
import Diagrams.Backend.SVG


domain :: String
domain = "jasonmc.net"

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
  { 
  providerDirectory    = "data/"
  }

dragon :: Int -> Trail V2 Double
dragon 0 = fromOffsets [unitX]
dragon n = dragon (n - 1) <> rotateBy (1/4) (reverseTrail $ dragon (n - 1))

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

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConfig $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "favicon/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    create ["diagram.svg"] $ do
        route idRoute
        compile diagramCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["CNAME"] $ do
        route idRoute
        compile $ makeItem domain


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
