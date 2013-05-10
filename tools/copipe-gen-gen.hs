{-# LANGUAGE NoImplicitPrelude #-}

import           FFI
import           JQuery  hiding (filter, not)
import           Prelude

-- FFIs

selfUrl :: String
selfUrl = ffi "location.href"

param :: String
param = ffi "location.search"

getParams :: [(String, String)]
getParams = concatMap (f . split "=") $ split "&" $ drop 1 param where
  f [k, v] = [(k, v)]
  f _ = []

split :: String -> String -> [String]
split = ffi "%2.split(%1)"

windowOpen :: String -> Fay ()
windowOpen = ffi "window.open(%1)"

uriEncode :: String -> String
uriEncode = ffi "encodeURIComponent(%1)"

uriDecode :: String -> String
uriDecode = ffi "decodeURIComponent(%1)"

setTitle :: String -> Fay ()
setTitle = ffi "document.title=%1"

tweet :: String -> Fay ()
tweet msg = windowOpen $
  "https://twitter.com/intent/tweet?text=" ++ uriEncode (msg ++ "\n") ++
  "&url=" ++ selfUrl

-- DOM

createElement :: String -> Fay Element
createElement = ffi "document.createElement(%1)"

createTextNode :: String -> Fay Element
createTextNode = ffi "document.createTextNode(%1)"

getElementById :: String -> Fay Element
getElementById = ffi "document.getElementById(%1)"

appendChild :: Element -> Element -> Fay ()
appendChild = ffi "%2.appendChild(%1)"

setId :: Element -> String -> Fay ()
setId = ffi "%1.id=%2"

getValOrPh :: JQuery -> Fay String
getValOrPh jq = do
  ret <- getVal jq
  if ret /= ""
    then return ret
    else getAttr "placeholder" jq

-- APP

data Template
  = Literal !String
  | Variable !String
  | Br
  deriving (Show)

parse :: String -> [Template]
parse = intercalate [Br] . map parse' . lines

parse' :: String -> [Template]
parse' "" = []
parse' ss =
  case spl "{{" ss of
    (a, b) -> case spl "}}" b  of
      (c, d) -> Literal a : Variable c : parse' d

spl :: String -> String -> (String, String)
spl needle "" = ("", "")
spl needle ss =
  case splitAt (length needle) ss of
    (pfx, rest) ->
      if pfx == needle then ("", rest)
        else case ss of
          (x:xs) -> case spl needle xs of
            (a, b) -> (x : a, b)

genTemplate :: [Template] -> Element -> Fay ()
genTemplate ots elm = f 0 ots where
  f _ [] = return ()
  f ix (Literal lit: ts) = do
    c <- createTextNode $ concatMap crToBr lit
    appendChild c elm
    f ix ts
  f ix (Variable "": ts) = f ix ts
  f ix (Variable var: ts) = do
    let iid = "plh-" ++ show ix
    c <- createElement "input"
    setId c iid
    appendChild c elm
    select ('#': iid)
      >>= setAttr "type" "text"
      >>= setAttr "width" (show $ length var)
      >>= setAttr "placeholder" var
      >>= setAttr "style" ("width:" ++ show (length var * 2) ++ "ex")
      >>= onChange (update ots)
      >>= keyup (const $ update ots)
    f (ix+1 :: Int) ts
  f ix (Br: ts) = do
    c <- createElement "br"
    appendChild c elm
    f ix ts

update :: [Template] -> Fay ()
update ts = do
  strs <- go 0 ts
  let str = concat strs
      len = length str
  select "#outputText" >>= setVal str
  updateCount "#outputText" id
  return ()
  where
    go _ [] = return []
    go ix (Literal lit: ts) = do
      rest <- go ix ts
      return $ lit : rest
    go ix (Variable "": ts) = go ix ts
    go ix (Variable var: ts) = do
      let iid = "plh-" ++ show ix
      rest <- go (ix+1 :: Int) ts
      cur <- select ('#': iid) >>= getValOrPh
      return $ cur : rest
    go ix (Br: ts) = do
      rest <- go ix ts
      return $ "\n" : rest

updateCount :: String -> (String -> String) -> Fay ()
updateCount txtId f = do
  str <- select txtId >>= getValOrPh
  let len = length $ f str
  select "[name=charCount]"
    >>= setText (show len)
    >>= setAttr "class" (if len <= 140 then "text-success" else "text-error")
  return ()

crToBr '\n' = "<br>"
crToBr c = [c]

main :: Fay ()
main = do
  case (lookup "title" getParams, lookup "tmpl" getParams) of
    (Just title, Just tmpl) -> do
      let genName = uriDecode title ++ "ジェネレータ"
          ptmpl   = parse $ uriDecode tmpl
      select "#tool-title" >>= setText genName
      getElementById "template-ctrls" >>= genTemplate ptmpl
      select "#use-tmpl" >>= setAttr "style" "display:block"
      setTitle genName
      select "#outputText"
        >>= onChange (updateCount "#outputText" id)
        >>= keyup (const $ updateCount "#outputText" id)
      update ptmpl

    _ -> do
      let f c = not $ c == '{' || c == '}'
      select "#template"
        >>= onChange (updateCount "#template" $ filter f)
        >>= keyup (const $ updateCount "#template" $ filter f)
      updateCount "#template" $ filter f
      select "#create-tmpl" >>= setAttr "style" "display:block"
      return ()

  select "#gen-gen" >>= onClick (\_ev -> do
    title <- select "#cpp-title" >>= getValOrPh
    tmpl  <- select "#template"  >>= getValOrPh
    windowOpen $ selfUrl ++ "?title=" ++ uriEncode title ++ "&tmpl=" ++ uriEncode tmpl
    return False
    )

  select "#tweet" >>= onClick (\_ev -> do
    select "#outputText" >>= getVal >>= tweet
    return False
    )

  return ()
