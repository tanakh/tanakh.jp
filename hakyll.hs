#! /usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Data.Monoid

import Hakyll
import Text.Pandoc
import Text.XHtml.Strict

main :: IO ()
main = hakyllWith config $ do
    match "img/*" $ do
      route idRoute
      compile copyFileCompiler

    match "js/**" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      -- compile compressCssCompiler
      compile copyFileCompiler

    match "google-code-prettify/*" $ do
      route idRoute
      compile copyFileCompiler
   
    match (list ["robots.txt", "humans.txt", "favicon.ico"]) $ do
      route idRoute
      compile copyFileCompiler
    
    -- Tags
    create "tags" $
      requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
      >>> arr tagsMap
      >>> arr (map (\(t, q) -> (tagIdentifier t, makeTagList t q)))

    match "templates/*" $ compile templateCompiler
      
    match "*.md" $ do
      route $ setExtension "html"
      compile $
        pageCompilerWithPandoc
          defaultHakyllParserState
          defaultHakyllWriterOptions { writerHtml5 = True }
          googlePrettify
        >>> applyTemplateCompiler "templates/default.hamlet"
        >>> relativizeUrlsCompiler

    match "posts/*.md" $ do
      route $ setExtension "html"
      compile $
        pageCompilerWithPandoc
          defaultHakyllParserState
          defaultHakyllWriterOptions { writerHtml5 = True }
          googlePrettify
        >>> arr (renderDateField "date" "%Y/%m/%e" "Date unknown")
        >>> renderTagsField "prettytags" (fromCapture "tags/*")
        >>> applyTemplateCompiler "templates/post.hamlet"
        >>> applyTemplateCompiler "templates/default.hamlet"
        >>> relativizeUrlsCompiler

    match "404.html" $ do
      route idRoute
      compile copyFileCompiler
   
    -- RSS
    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration

    -- index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
      >>> arr (setField "title" "Home")
      >>> requireA "tags" (setFieldA "tags" (renderTagList'))
      >>> setFieldPageList (take 3 . recentFirst)
            "templates/postitem.hamlet" "posts" "posts/*"
      >>> applyTemplateCompiler "templates/index.hamlet"
      >>> applyTemplateCompiler "templates/default.hamlet"

  where
    renderTagList' :: Compiler (Tags String) String
    renderTagList' = renderTagList tagIdentifier

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

makeTagList :: String
               -> [Page String]
               -> Compiler () (Page String)
makeTagList tagg posts =
  constA posts
  >>> pageListCompiler recentFirst "templates/postitem.hamlet"
  >>> arr (copyBodyToField "posts" . fromBody)
  >>> arr (setField "title" ("Posts tagged " ++ tagg))
  >>> applyTemplateCompiler "templates/posts.hamlet"
  >>> applyTemplateCompiler "templates/default.hamlet"

config :: HakyllConfiguration
config = defaultHakyllConfiguration { deployCommand = deploy }
  where
    deploy = "./hakyll.hs rebuild && cp -r _site/ ~/www/"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Purely Functional Space - a personal blog of tanakh"
    , feedDescription = "Personal blog of tanakh"
    , feedAuthorName = "Hideyuki Tanaka"
    , feedRoot = "http://tanakh.jp"
    }

googlePrettify :: Pandoc -> Pandoc
googlePrettify doc = bottomUp f doc where
  f (CodeBlock _ code) =
    RawBlock "html" $ "<pre class=\"prettyprint\">" ++ stringToHtmlString code ++ "</pre>"
  f b = b
