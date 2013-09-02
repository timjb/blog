--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>), mempty)
import           Hakyll
import           Text.Pandoc
import           Control.Applicative

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    match "graphics/*/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pdfs/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "CNAME" $ do
      route idRoute
      compile copyFileCompiler

    match (fromList ["about.markdown", "contact.markdown"]) $ do
         route   $ setExtension "html"
         compile $ pandocCompiler'

    match (fromRegex "posts/.*\\.(markdown|md|lhs)") $ do
        route $ setExtension "html"
        compile $ pandocCompiler'
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/*.html" $ do
        route $ setExtension "html"
        compile $ getResourceBody >>= applyAsTemplate defaultContext
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    create ["index.html"] $ do
        route $ setExtension "html"
        compile $ do
            posts <- take 8 <$> (recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion))
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Jonathan Sterling"                <>
                    field "about" (const $ loadBody "about.markdown") <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler

pandocOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

pandocCompiler' = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"   <>
    teaserField "teaser" "content" <>
    defaultContext

config :: Configuration
config = defaultConfiguration
    { deployCommand = "./deploy.sh" }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Jonathan Sterling"
    , feedDescription = "Jonathan Sterling writes about Types, Linguistics and Philology"
    , feedAuthorName  = "Jonathan Sterling"
    , feedAuthorEmail = "jonsterling@me.com"
    , feedRoot        = "http://www.jonmsterling.com/"
    }
