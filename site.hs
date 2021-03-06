#!/usr/bin/env stack
-- stack --resolver=lts-2.17 runghc --package hakyll

--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Data.Char   (toLower)
import           Data.Monoid
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match ("img/**" .||. "js/**" .||. "pub/**" .||. "robots.txt" .||. "favicon.ico" .||. "404.html" .||. "CNAME") $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.md", "contact.md", "pub.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*.md" (fromCapture "tags/*.html" . map toLower)

    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList tags "posts/*.md" recentFirst) <>
                    constField "title" "Archives" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "tools/*.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/tool.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "tools/*.js" $ do
        route   idRoute
        compile copyFileCompiler

    create ["tools.html"] $ do
        route idRoute
        compile $ do
            let toolsCtx =
                    field "posts" (\_ -> toolList "tools/*.html" recentFirst) <>
                    constField "title" "適当ツールズ" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" toolsCtx
                >>= loadAndApplyTemplate "templates/default.html" toolsCtx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html"
                        (constField "title" title <>
                            constField "posts" list <>
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                                postList tags "posts/*.md" $ fmap (take 5) . recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

--------------------------------------------------------------------------------
postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags pattern sortFilter = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl (postCtx tags) posts


--------------------------------------------------------------------------------
toolList :: Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
toolList pattern sortFilter = do
    tools   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/tool-item.html"
    applyTemplateList itemTpl defaultContext tools


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "cabal run deploy"
    }
