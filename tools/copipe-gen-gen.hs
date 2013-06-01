{-# LANGUAGE NoImplicitPrelude #-}

import           FFI
import           JQuery  hiding (filter, not)
import           Prelude

-- FFIs

selfUrl :: String
selfUrl = ffi "location.origin+location.pathname"

href :: String
href = ffi "location.href"

param :: String
param = ffi "location.search"

getParams :: [(String, String)]
getParams = concatMap (f . split "=") $ split "&" $ drop 1 param where
  f [k, v] = [(k, v)]
  f _ = []

location :: String -> Fay ()
location = ffi "document.location=%1"

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
  "&url=" ++ href

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

buttonLoading :: JQuery -> Fay JQuery
buttonLoading = ffi "(%1).button('loading')"

onEnter :: Fay () -> JQuery -> Fay JQuery
onEnter = ffi "(%2).keypress(function(e){if((e.which&&e.which==13)||(e.keyCode&&e.keyCode==13)){(%1)();}})"

-- Parse.com

initParse :: Fay ()
initParse = ffi "Parse.initialize('MgF3lCxSQnmSdpyGRyAJdAYq8VSAvQw0diWiX9Yl','RROYSWTenBgSCL9LJZavM6AIk8MVhoaibizr1PWz')"

data CopipeObject

newObject :: Fay CopipeObject
newObject = ffi "(function(){var o=Parse.Object.extend('CopipeObject');var r=new o();return r;})()"

save :: CopipeObject -> Automatic a -> (CopipeObject -> Fay ()) -> Fay ()
save = ffi "%1.save(%2, {success: (%3)})"

data CopipeQuery

newQuery :: Fay CopipeQuery
newQuery = ffi "new Parse.Query(Parse.Object.extend('CopipeObject'))"

getQuery :: CopipeQuery -> String -> (CopipeGenerator -> Fay ()) -> Fay () -> Fay ()
getQuery = ffi "(%1).get(%2, {success: function(object){object.increment('pv',1);object.save();(%3)(object.toJSON())}, error: function(object, err){(%4)()}})"

desc :: String -> CopipeQuery -> Fay CopipeQuery
desc = ffi "(%2).descending(%1)"

contains :: String -> String -> CopipeQuery -> Fay CopipeQuery
contains = ffi "(%3).contains(%1,%2)"

orQuery :: CopipeQuery -> CopipeQuery -> Fay CopipeQuery
orQuery = ffi "Parse.Query.or(%1,%2)"

pQuery :: Int -> Int -> (String -> CopipeGenerator -> Fay ()) -> CopipeQuery -> Fay ()
pQuery = ffi "(%4).greaterThan('pv',0).skip(%1).limit(%2).find({success: function(results){for(var i=0;i<results.length;i++){(%3)(results[i].toJSON()['objectId'],results[i].toJSON())}}})"

pCount :: (Int -> Fay ()) -> CopipeQuery -> Fay ()
pCount = ffi "(%2).greaterThan('pv',0).count({success:function(num){(%1)(num);}})"

-- aux

get :: a -> String -> Fay String
get = ffi "(%1)[%2]"

alert :: JQuery -> Fay ()
alert = ffi "(%1).alert()"

popover :: String -> String -> JQuery -> Fay ()
popover = ffi "(%3).popover({title:%1,content:%2}).popover('show')"

unless p = when (not p)

unbind :: JQuery -> Fay JQuery
unbind = ffi "(%1).unbind()"

onClick' :: String -> (Fay a) -> Fay ()
onClick' selector h = do
  select selector >>= unbind
  ((select selector >>=) . onClick) (\_ev -> h >> return False)
  return ()

-- APP

data CopipeGenerator
  = CopipeGenerator
    { title    :: String
    , author   :: String
    , template :: String
    }
    deriving (Show)

data Template
  = Literal !String
  | Variable !String
  | Br

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
  initParse
  case lookup "id" getParams of
    Nothing -> gengen
    Just gid -> do
      q <- newQuery
      getQuery q gid generator (location selfUrl)

  let addLink eid pfx id cg = do
        elm <- getElementById eid
        let pid = pfx ++ id
        li <- createElement "li"
        c <- createElement "a"
        setId c pid
        appendChild c li
        appendChild li elm
        select ('#':pid) >>= setText (title cg) >>= setAttr "href" (selfUrl ++ "?id=" ++ id)
        return ()

  let pager sel pfx cnt query = go 0 10 where
        go cur ppp = do
          let maxCur = max 0 $ (cnt + ppp - 1) `div` ppp * ppp - ppp
          select ("#"++sel++" *") >>= remove
          query >>= pQuery cur ppp (addLink sel pfx)
          onClick' ("#"++pfx++"init") $ go 0 ppp
          onClick' ("#"++pfx++"prev") $ go (max 0 $ cur - ppp) ppp
          onClick' ("#"++pfx++"next") $ go (min maxCur $ cur + ppp) ppp
          onClick' ("#"++pfx++"last") $ go maxCur ppp

  ((newQuery >>=) . pCount) $ \cnt -> do
    pager "popular" "pop-" cnt $ newQuery >>= desc "pv"
    pager "recent"  "rec-" cnt $ newQuery >>= desc "updateAt"

  let txtSearch = do
        q <- select "#query-text" >>= getVal
        when (q /= "") $
          pager "search-result" "src-" 99999 $ do
            a <- newQuery >>= contains "title" q
            b <- newQuery >>= contains "template" q
            orQuery a b

  onClick' "#txt-search" txtSearch
  select "#query-text" >>= onEnter txtSearch

  onClick' "#author-search" $ do
    q <- select "#query-text" >>= getVal
    when (q /= "") $
      pager "search-result" "src-" (-1) $
        newQuery >>= contains "author" q

generator cg = do
  let genName = uriDecode (title cg) ++ "ジェネレータ"
      ptmpl   = parse $ uriDecode $ template cg
  select "#tool-title" >>= setText genName >>= setAttr "style" "display:block"
  getElementById "template-ctrls" >>= genTemplate ptmpl

  when (author cg /= "") $
    getElementById "presented-by" >>= \pby -> do
      pre <- createTextNode "by @"
      appendChild pre pby
      a <- createElement "a"
      setId a "author-id"
      appendChild a pby
      select "#author-id"
        >>= setText (author cg)
        >>= setProp "href" ("https://twitter.com/" ++ author cg)

  select "#use-tmpl" >>= setAttr "style" "display:block"
  setTitle genName
  select "#outputText"
    >>= onChange (updateCount "#outputText" id)
    >>= keyup (const $ updateCount "#outputText" id)
  update ptmpl

  onClick' "#tweet" $ select "#outputText" >>= getVal >>= tweet
  onClick' "#ret"   $ location selfUrl

  return ()

gengen = do
  select "#tool-title" >>= setAttr "style" "display:block"

  let f c = not $ c == '{' || c == '}'
  select "#template"
    >>= onChange (updateCount "#template" $ filter f)
    >>= keyup (const $ updateCount "#template" $ filter f)
  updateCount "#template" $ filter f

  onClick' "#gen-gen" $ do
    title <- select "#cpp-title" >>= getValOrPh
    auth  <- select "#author" >>= getVal
    tmpl  <- select "#template"  >>= getValOrPh

    let invTitle = length title >= 50
        invAuth  = length auth  >= 30
        invTmpl  = length tmpl  >= 999

    when invTitle $ select "#cpp-title"
      >>= popover "タイトルが長すぎます" "タイトルは50文字までです"
    when invAuth  $ select "#author"
      >>= popover "作者名が長すぎます" "作者名は30文字までです（空欄でも可）"
    when invTmpl  $ select "#template"
      >>= popover "テンプレートが長すぎます" "テンプレートは999文字までです"

    unless (invTitle || invAuth || invTmpl) $ do
      select "#gen-gen" >>= buttonLoading
      obj <- newObject
      save obj (CopipeGenerator title auth tmpl) $ \obj -> do
        objId <- get obj "id"
        location $ selfUrl ++ "?id=" ++ objId

  select "#create-tmpl" >>= setAttr "style" "display:block"
  return ()
