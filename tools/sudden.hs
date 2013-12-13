{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Fay.Text as T
import           FFI
import           JQuery
import           Prelude

(<>) = T.append

totsuzenize :: String -> String
totsuzenize str = unlines $ [hdr] ++ ctr ++ [ftr]
  where
    strs = lines str
    w = maximum $ map width strs
    adjs = map (adjust w) strs

    hdr = "＿" ++ cr (w + 2) "人" ++ "＿"
    ctr = map (\ss -> "＞　" ++ ss ++ "　＜") adjs
    ftr = "￣Y" ++ cr w "^Y" ++ "￣"

    adjust w ss = p ++ ss ++ f where
      v = width ss
      m = w - v
      p = cr (m `div` 2) "　" ++ cr (m `mod` 2) " "
      f = cr (m `mod` 2) " " ++ cr (m `div` 2) "　"

    cr n = concat . replicate n

    width ss = (sum (map len ss) + 1 ) `div` 2
    len c
      | c <= '\x7f' = 1
      | otherwise = 2

selfUrl :: T.Text
selfUrl = ffi "location.href"

windowOpen :: T.Text -> Fay ()
windowOpen = ffi "window.open(%1)"

uriEncode :: T.Text -> T.Text
uriEncode = ffi "encodeURIComponent(%1)"

tweet :: T.Text -> Fay ()
tweet msg = windowOpen $
  "https://twitter.com/intent/tweet?text=" <> uriEncode (msg <> "\n") <>
  "&url=" <> selfUrl

main :: Fay ()
main = do
  input  <- select $ T.pack "#inputText"
  output <- select $ T.pack "#outputText"

  let gen = do
        str <- getVal input
        setVal (T.pack $ totsuzenize $ T.unpack str) output
        return ()

  keyup (const gen) input
  onChange gen input

  select (T.pack "#tweet") >>= onClick (\_ev -> do
    tweet =<< getVal output
    return False
    )

  return ()
