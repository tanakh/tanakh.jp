{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Shelly
import qualified Data.Text as T
import Control.Monad

default (T.Text)

git_ :: T.Text -> [T.Text] -> Sh ()
git_ = command1_ "git" []

main :: IO ()
main = shelly $ do
  run_ "cabal" ["run", "tanakh-jp", "rebuild"]
  git_ "checkout" ["gh-pages"]
  run_ "cp" ["-r", "_site/*", "."]

  files <- findWhen test_f "_site"
  forM_ files $ \file -> do
    t <- toTextWarn file
    git_ "add" [t]

  git_ "commit" ["-m", "update"]
  git_ "push" []
  git_ "checkout" ["master"]
