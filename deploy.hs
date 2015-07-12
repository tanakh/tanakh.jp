#!/usr/bin/env stack
-- stack --resolver=lts-2.17 runghc --package hakyll

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import           Control.Monad
import qualified Data.Text     as T
import           Shelly

default (T.Text)

git_ :: T.Text -> [T.Text] -> Sh ()
git_ = command1_ "git" []

main :: IO ()
main = shelly $ do
  git_ "checkout" ["master"]
  run_ "cabal" ["run", "tanakh-jp", "rebuild"]
  git_ "checkout" ["gh-pages"]

  files <- lsT "_site"
  forM_ files $ \file -> do
    run_ "cp" ["-r", file, "."]

  files <- findWhen test_f "_site"
  forM_ files $ \file -> do
    t <- toTextWarn file
    git_ "add" [T.drop 6 t]

  git_ "commit" ["-m", "update"]
  git_ "push" []

  `finally_sh` git_ "checkout" ["master"]
