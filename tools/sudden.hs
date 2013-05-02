{-# LANGUAGE NoImplicitPrelude #-}

import           FFI
import           JQuery
import           Prelude

totsuzenize :: String -> String
totsuzenize str = unlines [hdr, ctr, ftr]
  where
    hdr = "＿" ++ concat (replicate (w + 2) "人") ++ "＿"
    ctr = "＞　" ++ str ++ "　＜"
    ftr = "￣Y" ++ concat (replicate w "^Y") ++ "￣"

    w = (sum (map len str) + 1 ) `div` 2
    len c
      | c <= '\x7f' = 1
      | otherwise = 2

selfUrl :: String
selfUrl = ffi "location.href"

windowOpen :: String -> Fay ()
windowOpen = ffi "window.open(%1)"

uriEncode :: String -> String
uriEncode = ffi "encodeURIComponent(%1)"

tweet :: String -> Fay ()
tweet msg = windowOpen $
  "https://twitter.com/intent/tweet?text=" ++ uriEncode (msg ++ "\n") ++
  "&url=" ++ selfUrl

main :: Fay ()
main = do
  input  <- select "#inputText"
  output <- select "#outputText"

  let gen = do
        str <- getVal input
        setVal (totsuzenize str) output
        return ()

  keyup (const gen) input
  onChange gen input

  select "#tweet" >>= onClick (\_ev -> do
    tweet =<< getVal output
    return False
    )

  return ()
