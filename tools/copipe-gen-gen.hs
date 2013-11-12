{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Fay.Text as T
import           FFI
import           JQuery   hiding (filter, not)
import           Prelude

-- FFIs

selfUrl :: T.Text
selfUrl = ffi "location.origin+location.pathname"

href :: T.Text
href = ffi "location.href"

param :: T.Text
param = ffi "location.search"

getParams :: [(T.Text, T.Text)]
getParams = concatMap (f . split "=") $ split "&" $ T.pack $ drop 1 $ T.unpack param where
  f [k, v] = [(k, v)]
  f _ = []

location :: T.Text -> Fay ()
location = ffi "document.location=%1"

split :: T.Text -> T.Text -> [T.Text]
split = ffi "%2.split(%1)"

windowOpen :: T.Text -> Fay ()
windowOpen = ffi "window.open(%1)"

uriEncode :: T.Text -> T.Text
uriEncode = ffi "encodeURIComponent(%1)"

uriDecode :: T.Text -> T.Text
uriDecode = ffi "decodeURIComponent(%1)"

setTitle :: T.Text -> Fay ()
setTitle = ffi "document.title=%1"

tweet :: T.Text -> Fay ()
tweet msg = windowOpen $
  "https://twitter.com/intent/tweet?text=" <> uriEncode (msg <> "\n") <>
  "&url=" <> href

-- DOM

createElement :: T.Text -> Fay Element
createElement = ffi "document.createElement(%1)"

createTextNode :: T.Text -> Fay Element
createTextNode = ffi "document.createTextNode(%1)"

getElementById :: T.Text -> Fay Element
getElementById = ffi "document.getElementById(%1)"

appendChild :: Element -> Element -> Fay ()
appendChild = ffi "%2.appendChild(%1)"

setId :: Element -> T.Text -> Fay ()
setId = ffi "%1.id=%2"

getValOrPh :: JQuery -> Fay T.Text
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

getQuery :: CopipeQuery -> T.Text -> (CopipeGenerator -> Fay ()) -> Fay () -> Fay ()
getQuery = ffi "(%1).get(%2, {success: function(object){object.increment('pv',1);object.save();(%3)(object.toJSON())}, error: function(object, err){(%4)()}})"

desc :: T.Text -> CopipeQuery -> Fay CopipeQuery
desc = ffi "(%2).descending(%1)"

contains :: T.Text -> T.Text -> CopipeQuery -> Fay CopipeQuery
contains = ffi "(%3).contains(%1,%2)"

orQuery :: CopipeQuery -> CopipeQuery -> Fay CopipeQuery
orQuery = ffi "Parse.Query.or(%1,%2)"

pQuery :: Int -> Int -> (T.Text -> CopipeGenerator -> Fay ()) -> CopipeQuery -> Fay ()
pQuery = ffi "(%4).greaterThan('pv',0).skip(%1).limit(%2).find({success: function(results){for(var i=0;i<results.length;i++){(%3)(results[i].toJSON()['objectId'],results[i].toJSON())}}})"

pCount :: (Int -> Fay ()) -> CopipeQuery -> Fay ()
pCount = ffi "(%2).greaterThan('pv',0).count({success:function(num){(%1)(num);}})"

-- aux

(<>) = T.append

filter' :: (Char -> Bool) -> T.Text -> T.Text
filter' f = T.pack . filter f . T.unpack

get :: a -> T.Text -> Fay T.Text
get = ffi "(%1)[%2]"

alert :: JQuery -> Fay ()
alert = ffi "(%1).alert()"

popover :: T.Text -> T.Text -> JQuery -> Fay ()
popover = ffi "(%3).popover({title:%1,content:%2}).popover('show')"

unless p = when (not p)

unbind :: JQuery -> Fay JQuery
unbind = ffi "(%1).unbind()"

onClick' :: T.Text -> (Fay a) -> Fay ()
onClick' selector h = do
  select selector >>= unbind
  ((select selector >>=) . onClick) (\_ev -> h >> return False)
  return ()

-- APP

data CopipeGenerator
  = CopipeGenerator
    { title    :: T.Text
    , author   :: T.Text
    , template :: T.Text
    }
    deriving (Show)

data Template
  = Literal !T.Text
  | Variable !T.Text
  | Br

parse :: T.Text -> [Template]
parse = intercalate [Br] . map parse' . lines . T.unpack

parse' :: String -> [Template]
parse' "" = []
parse' ss =
  case spl "{{" ss of
    (a, b) -> case spl "}}" b  of
      (c, d) -> Literal (T.pack a) : Variable (T.pack c) : parse' d

spl :: String -> String -> (String, String)
spl needle "" = ("", "")
spl needle ss =
  case splitAt (length needle) ss of
    (pfx, rest) ->
      if pfx == needle then ("", rest)
        else case ss of
          (x:xs) -> case spl needle xs of
            (a, b) -> (x:a, b)

genTemplate :: [Template] -> Element -> Fay ()
genTemplate ots elm = f 0 ots where
  f _ [] = return ()
  f ix (Literal lit: ts) = do
    c <- createTextNode $ T.concatMap crToBr lit
    appendChild c elm
    f ix ts
  f ix (Variable "": ts) = f ix ts
  f ix (Variable var: ts) = do
    let iid = "plh-" <> T.pack (show ix)
    c <- createElement "input"
    setId c iid
    appendChild c elm
    select ("#" <> iid)
      >>= setAttr "type" "text"
      >>= setAttr "width" (T.pack $ show $ T.length var)
      >>= setAttr "placeholder" var
      >>= setAttr "style" ("width:" <> T.pack (show (T.length var * 2)) <> "ex")
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
  let str = T.concat strs
      len = T.length str
  select (T.pack "#outputText") >>= setVal str
  updateCount "#outputText" id
  return ()
  where
    go _ [] = return []
    go ix (Literal lit: ts) = do
      rest <- go ix ts
      return $ lit : rest
    go ix (Variable "": ts) = go ix ts
    go ix (Variable var: ts) = do
      let iid = "plh-" <> T.pack (show ix)
      rest <- go (ix+1 :: Int) ts
      cur <- select ("#" <> iid) >>= getValOrPh
      return $ cur : rest
    go ix (Br: ts) = do
      rest <- go ix ts
      return $ "\n" : rest

updateCount :: T.Text -> (T.Text -> T.Text) -> Fay ()
updateCount txtId f = do
  str <- select txtId >>= getValOrPh
  let len = T.length $ f str
  select (T.pack "[name=charCount]")
    >>= setText (T.pack $ show len)
    >>= setAttr "class" (if len <= 140 then "text-success" else "text-error")
  return ()

crToBr '\n' = "<br>"
crToBr c = T.pack [c]

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
        let pid = pfx <> id
        li <- createElement "li"
        c <- createElement "a"
        setId c pid
        appendChild c li
        appendChild li elm
        select ("#"<>pid) >>= setText (title cg) >>= setAttr "href" (selfUrl <> "?id=" <> id)
        return ()

  let pager sel pfx cnt query = go 0 10 where
        go cur ppp = do
          let maxCur = max 0 $ (cnt + ppp - 1) `div` ppp * ppp - ppp
          select ("#"<>sel<>" *") >>= remove
          query >>= pQuery cur ppp (addLink sel pfx)
          onClick' ("#"<>pfx<>"init") $ go 0 ppp
          onClick' ("#"<>pfx<>"prev") $ go (max 0 $ cur - ppp) ppp
          onClick' ("#"<>pfx<>"next") $ go (min maxCur $ cur + ppp) ppp
          onClick' ("#"<>pfx<>"last") $ go maxCur ppp

  ((newQuery >>=) . pCount) $ \cnt -> do
    pager "popular" "pop-" cnt $ newQuery >>= desc "pv"
    pager "recent"  "rec-" cnt $ newQuery >>= desc "updatedAt"

  let txtSearch = do
        q <- select (T.pack "#query-text") >>= getVal
        when (q /= "") $
          pager "search-result" "src-" 99999 $ do
            a <- newQuery >>= contains "title" q
            b <- newQuery >>= contains "template" q
            orQuery a b

  onClick' "#txt-search" txtSearch
  select (T.pack "#query-text") >>= onEnter txtSearch

  onClick' "#author-search" $ do
    q <- select (T.pack "#query-text") >>= getVal
    when (q /= "") $
      pager "search-result" "src-" (-1) $
        newQuery >>= contains "author" q

generator cg = do
  let genName = uriDecode (title cg) <> "ジェネレータ"
      ptmpl   = parse $ uriDecode $ template cg
  select (T.pack "#tool-title") >>= setText genName >>= setAttr "style" "display:block"
  getElementById "template-ctrls" >>= genTemplate ptmpl

  when (author cg /= "") $
    getElementById "presented-by" >>= \pby -> do
      pre <- createTextNode "by @"
      appendChild pre pby
      a <- createElement "a"
      setId a "author-id"
      appendChild a pby
      select (T.pack "#author-id")
        >>= setText (author cg)
        >>= setProp "href" ("https://twitter.com/" <> author cg)

  select (T.pack "#use-tmpl") >>= setAttr "style" "display:block"
  setTitle genName
  select (T.pack "#outputText")
    >>= onChange (updateCount "#outputText" id)
    >>= keyup (const $ updateCount "#outputText" id)
  update ptmpl

  onClick' "#tweet" $ select (T.pack "#outputText") >>= getVal >>= tweet
  onClick' "#ret"   $ location selfUrl

  return ()

gengen = do
  select (T.pack "#tool-title") >>= setAttr "style" "display:block"

  let f c = not $ c == '{' || c == '}'
  select (T.pack "#template")
    >>= onChange (updateCount "#template" $ filter' f)
    >>= keyup (const $ updateCount "#template" $ filter' f)
  updateCount "#template" $ filter' f

  onClick' "#gen-gen" $ do
    title <- select (T.pack "#cpp-title") >>= getValOrPh
    auth  <- select (T.pack "#author")    >>= getVal
    tmpl  <- select (T.pack "#template")  >>= getValOrPh

    let invTitle = T.length title >= 50
        invAuth  = T.length auth  >= 30
        invTmpl  = T.length tmpl  >= 999

    when invTitle $ select (T.pack "#cpp-title")
      >>= popover "タイトルが長すぎます" "タイトルは50文字までです"
    when invAuth  $ select (T.pack "#author")
      >>= popover "作者名が長すぎます" "作者名は30文字までです（空欄でも可）"
    when invTmpl  $ select (T.pack "#template")
      >>= popover "テンプレートが長すぎます" "テンプレートは999文字までです"

    unless (invTitle || invAuth || invTmpl) $ do
      select (T.pack "#gen-gen") >>= buttonLoading
      obj <- newObject
      save obj (CopipeGenerator title auth tmpl) $ \obj -> do
        objId <- get obj "id"
        location $ selfUrl <> "?id=" <> objId

  select (T.pack "#create-tmpl") >>= setAttr "style" "display:block"
  return ()
