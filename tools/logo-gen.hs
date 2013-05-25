{-# LANGUAGE NoImplicitPrelude #-}

import           FFI
import           JQuery
import           Prelude

-- flickr Photo search api
-- http://www.flickr.com/services/api/flickr.photos.search.html

appkey = "b48b6c86898dd92d69df9f1f00ec3f09"
appsecret = "b0a7af602b27c030"

endpoint = "http://api.flickr.com/services"

data Photo

photoUrl :: Photo -> String
photoUrl = ffi "'http://www.corsproxy.com/farm'+%1.farm+'.staticflickr.com/'+%1.server+'/'+%1.id+'_'+%1.secret+'_b.jpg'"

data Canvas
data Context

getCanvas :: String -> Fay Canvas
getCanvas = ffi "document.getElementById(%1)"

getContext :: String -> Fay Context
getContext = ffi "(function(){var c=document.getElementById(%1);var ctx=c.getContext('2d');return ctx;})()"

line :: Double -> Double -> Double -> Double -> Context -> Fay ()
line = ffi "(function(){%5.shadowColor='rgb(255,255,255)';%5.shadowBlur=6;%5.lineWidth=Math.max(1,(%3-%2)/170);%5.beginPath();%5.moveTo(%1,%2);%5.lineTo(%3,%4);%5.stroke();})()"

text :: Double -> Double -> String -> Double -> Context -> Fay ()
text = ffi "(function(){%5.font='bold '+(%4)+'pt Takao';%5.shadowColor='rgb(255,255,255)';%5.shadowBlur=Math.min(15,%4/8);%5.fillText(%3,%1,%2);})()"

measureWidth :: String -> Double -> Context -> Fay Double
measureWidth = ffi "(function(){%3.font=(%2)+'pt Takao';var m=%3.measureText(%1);return m.width})()"

measureHeight :: String -> Double -> Context -> Fay Double
measureHeight = ffi "(function(){%3.font=(%2)+'pt Takao';var m=%3.measureText(%1);return m.height})()"

image :: Double -> Double -> String -> Context -> Fay () -> Fay ()
image = ffi "(function(){var img=new Image();img.crossOrigin='Anonymous';img.onload=function(){var scl=Math.min(img.width/%1,img.height/%2),ww=img.width/scl,hh=img.height/scl,dx=(ww-%1)/2,dy=(hh-%2)/2;console.log(img.width, img.height,%1, %2,ww,hh,scl,dx,dy);%4.drawImage(img,0,0,img.width,img.height,-dx,-dy,%1+dx,%2+dy);(%5)();};img.src=%3;})()"

rnd :: Double -> Double -> Fay Double
rnd = ffi "Math.random()*(%2-%1)+%1"

rndPhoto :: Object -> Fay Photo
rndPhoto = ffi "%1.photos.photo[Math.floor(Math.random()*%1.photos.photo.length)]"

isHiragana :: Char -> Bool
isHiragana = ffi "%1.match(/^[\\u3040-\\u309F]+$/)"

getValOrPh :: JQuery -> Fay String
getValOrPh jq = do
  ret <- getVal jq
  if ret /= ""
    then return ret
    else getAttr "placeholder" jq

windowOpen :: String -> Fay ()
windowOpen = ffi "window.open(%1)"

toDataUrl :: Canvas -> Fay String
toDataUrl = ffi "%1.toDataURL()"

twitterUploadUrl = "https://upload.twitter.com/1/statuses/update_with_media.json"

createReq :: String -> String -> Fay Object
createReq = ffi "{'status':%1, 'media[]': (%2).split(',')[1]}"

fillRect :: String -> Double -> Double -> Context -> Fay ()
fillRect = ffi "(function(){%4.save();%4.fillStyle=%1;%4.fillRect(0,0,%2,%3);%4.restore()})()"

toDouble :: String -> Double
toDouble = ffi "parseFloat(%1)"

btnLoading :: JQuery -> Fay ()
btnLoading = ffi "%1.button('loading')"

btnReset :: JQuery -> Fay ()
btnReset = ffi "%1.button('reset')"

clearCanvas :: String -> Fay ()
clearCanvas cid = do
  w <- select ("#" ++ cid) >>= getAttr "width"
  h <- select ("#" ++ cid) >>= getAttr "height"
  getContext cid >>= fillRect "rgb(127,127,127)" (toDouble w) (toDouble h)

mapM :: (a -> Fay b) -> [a] -> Fay [b]
mapM _ [] = return []
mapM f (x:xs) = do
  r <- f x
  rs <- mapM f xs
  return $ r : rs

isDigit c = '0' <= c && c <= '9'

drawLogo :: Double -> Double -> String -> String -> Context -> Fay ()
drawLogo rwidth rheight ss subtitle ctx = do
  let def = 40
  let hscs = map (\c -> if isHiragana c then 0.7 else 1.0) ss
  rsz <- mapM (\_c -> rnd 0.8 1.0) ss
  let sz = zipWith (*) (1 : repeat 0.8) $ zipWith (*) rsz hscs
  mets <- mapM (\(c,s) -> measureWidth [c] (s * 40) ctx) $ zip ss sz
  let mwidth = sum mets + fromIntegral (length mets - 1) * 0.1
      scl = rwidth * 0.9 / mwidth
      ypos = rheight * 0.5

      go cs szs mts xpos first = case (cs, szs, mts) of
        (c:cs, sz:szs, m:mts) -> do
          dy <- rnd 0.2 0.5
          dy <- if first then return 0 else return dy
          text xpos (ypos - dy * scl * def) [c] (sz*scl*40) ctx
          when first $ do
            line (xpos + m *scl + 0.2 * def * scl) ypos (rwidth * 0.92) ypos ctx
          go cs szs mts (xpos + m * scl) False
        _ -> return ()

  go ss sz mets (rwidth * 0.05) True

  let ssize = def * scl * 0.2
  swidth <- measureWidth subtitle ssize ctx
  sheight <- measureHeight subtitle ssize ctx

  text (rwidth*0.85-swidth) (ypos + ssize * 1.3) subtitle ssize ctx

main :: Fay ()
main = do
  select "#generate" >>= onClick (\_ev -> do
    select "#generate" >>= btnLoading

    theme <- select "#bg-image" >>= getValOrPh

    let url = endpoint
              ++ "/rest"
              ++ "?method=flickr.photos.search"
              ++ "&api_key=" ++ appkey
              ++ "&text=" ++ theme
              ++ "&sort=interestingness-desc"
              -- ++ "&license=1"
              ++ "&safe_search=1"
              ++ "&content_type=1"
              ++ "&media=photos"
              ++ "&format=json&nojsoncallback=1"
    ctx <- getContext "cvs"

    logo <- select "#title-logo" >>= getValOrPh
    subtitle <- select "#subtitle" >>= getValOrPh

    width <- select "#cvs" >>= getAttr "width"
    height <- select "#cvs" >>= getAttr "height"

    let cb photos = do
          p <- rndPhoto photos
          image (toDouble width) (toDouble height) (photoUrl p) ctx $ do
            drawLogo (toDouble width) (toDouble height) logo subtitle ctx
            select "#generate" >>= btnReset

    ajax url cb (\_fail _x _y -> return ())
    return False
    )

  select "#save" >>= onClick (\_ev -> do
    windowOpen =<< toDataUrl =<< getCanvas "cvs"
    return False
    )

  select "#size" >>= onChange (do
    txt <- select "#size option:selected" >>= getText
    case words $ map (\c -> if isDigit c then c else ' ') txt of
      [w, h] -> do
        select "#cvs" >>= setAttr "width" w >>= setAttr "height" h
        clearCanvas "cvs"
    return ()
    )

  clearCanvas "cvs"

{-
  select "#tweet" >>= onClick (\_ev -> do
    logo <- select "#title-logo" >>= getValOrPh
    subtitle <- select "#subtitle" >>= getValOrPh

    req <- createReq (logo ++ " - " ++ subtitle) =<< toDataUrl =<< getElementById "cvs"
    ajaxPost twitterUploadUrl req (\_ -> return ()) (\_ _ _ -> return ())
    return False
    )
-}

  return ()
