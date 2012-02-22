#! /usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Data.Monoid

import Hakyll

main :: IO ()
main = hakyllWith config $ do
  -- Tags
  create "tags" $
    requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

  -- Add a tag list compiler for every tag
  match "tags/*" $ route $ setExtension ".html"
  metaCompile $ require_ "tags"
    >>> arr tagsMap
    >>> arr (map (\(t, q) -> (tagIdentifier t, makeTagList t q)))

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

  -- pages
  match (list ["pub.md", "about.md"]) $ do
    route $ setExtension "html"
    compile $
      pageCompilerWithPandoc
        defaultHakyllParserState
        defaultHakyllWriterOptions
        id
      >>> applyTemplateCompiler "templates/default.hamlet"
      >>> relativizeUrlsCompiler
    
  -- blog posts
  match "posts/*.md" $ do
    route $ setExtension "html"
    compile $
      pageCompilerWithPandoc
        defaultHakyllParserState
        defaultHakyllWriterOptions
        id
      >>> arr (renderDateField "date" "%Y/%m/%e" "Date unknown")
      >>> arr (renderDateField "d_year" "%Y" "Date unknown")
      >>> arr (renderDateField "d_month" "%b" "Date unknown")
      >>> arr (renderDateField "d_date" "%e" "Date unknown")
      >>> renderTagsField "prettytags" (fromCapture "tags/*")
      >>> applyTemplateCompiler "templates/post.hamlet"
      >>> applyTemplateCompiler "templates/default.hamlet"
      >>> relativizeUrlsCompiler

  -- templates
  match "templates/*" $ compile templateCompiler
      
  -- static contents
  match "img/**" $ do
    route idRoute
    compile copyFileCompiler

  match "js/**" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (list ["robots.txt", "humans.txt", "favicon.ico", "404.html", "50x.html"]) $ do
    route idRoute
    compile copyFileCompiler

  match "pub/**" $ do
    route idRoute
    compile copyFileCompiler
    
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
config = defaultHakyllConfiguration
  { deployCommand =
       "./hakyll.hs rebuild && \
       \cp -r _site/ ~/Dropbox/www/tanakh.jp/"
  }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "Purely Functional Space - a personal blog of tanakh"
  , feedDescription = "Personal blog of tanakh"
  , feedAuthorName  = "Hideyuki Tanaka"
  , feedRoot        = "http://tanakh.jp"
  }
