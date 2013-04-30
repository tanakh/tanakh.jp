--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char   (toLower)
import           Data.Monoid (mappend, mconcat)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match ("img/**" .||. "js/**" .||. "pub/**" .||. "robots.txt" .||. "favicon.ico" .||. "404.html") $ do
        route idRoute
        compile copyFileCompiler


    match (fromList ["about.md", "contact.md", "pub.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html" . map toLower)

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList tags "posts/*" recentFirst) `mappend`
                    constField "title" "Archives"              `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
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
                        (constField "title" title `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                                postList tags "posts/*" $ fmap (take 3) . recentFirst

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
config :: Configuration
config = defaultConfiguration
    { deployCommand = "cp -r _site /c/Users/tanakh/Dropbox/www/tanakh.jp/"
    }
