---
title: 非決定計算としてのリストモナド
description: リストモナドは何かを列挙する問題に便利
tags: Haskell
---

## 最近のHaskell

最近はHaskellに関するブログ記事をちらほら見かけるようになって来ました。
日々のスクリプトに使おうとしようという人もいるようで、

<http://d.hatena.ne.jp/aya_eiya/20120221/1329836207>

このようなブログ記事を見かけました。
Haskellで書いたらそんなに汚いコードにはならないというのはたしかにあります。
やってはならないということがHaskellには比較的少ないのだと思います。
というのも、Haskellでは、やってはいけないことがそもそもできないようになっているか、
やることが極めて難しくなっている（unsafeプリフィックスを持つ関数を駆使したりなど）
ことが多いのです。

しかし、そんなHaskellのプログラムも、少し工夫すればもっと良くなることもあります。

## 列挙問題

先ほどのブログの記事の1つ目のプログラムを見てみましょう。

<script src="https://gist.github.com/1875172.js?file=getAllUpperLowerPattern.hs"></script>

このプログラムは、与えられた文字列の書く文字を大文字、小文字にした各パターンを全て返すものです。

~~~ {.haskell}
> getAllUpperLowerPattern "ABCD"
["ABCD","ABCd","ABcD","ABcd","AbCD","AbCd","AbcD","Abcd","aBCD","aBCd","aBcD","aBcd","abCD","abCd","abcD","abcd"]
~~~

このプログラムはこのままでも特に問題は無いのですが、
難点があるとすれば少しややこしいということでしょうか。
コードを読んでみると次のようになっています。

* 一行目、01のパターンを生成
    `["0000","0001","0010","0011","0100","0101","0110","0111","1000","1001","1010","1011","1100","1101","1110","1111"]`

* 二行目、各01と文字をペアに
    `[[('0','A'),('0','B'),('0','C'),('0','D')],[('0','A'),('0','B'),('0','C'),('1','D')],[('0','A'),('0','B'),('1','C'),('0','D')],[('0','A'),('0','B'),('1','C'),('1','D')],[('0','A'),('1','B'),('0','C'),('0','D')], ...`

* 三行目、0とペアになった文字をtoLower、1とペアになった文字をtoUpperする
    `["ABCD","ABCd","ABcD","ABcd","AbCD","AbCd","AbcD","Abcd","aBCD","aBCd","aBcD","aBcd","abCD","abCd","abcD","abcd"]`

## 関数をそのまま使う

このような三段階を踏んでいます。文字 `'0'`, `'1'` をそれぞれtoUpper, toLowerに関連付けていますが、
関数型言語なので、toUpper、toLowerをそのまま使えます。

~~~ {.haskell}
getAllUpperLowerPattern str = f3
  where
    f1 = sequence $ replicate (length str) [toUpper, toLower]
    f2 = map (\(fs,str)->zip fs str) $ zip f1 (replicate (length f1) str)
    f3 = nub [a|cs<-f2,a<-[[f c|(f,c)<-cs]]]
~~~

## リストの生成の仕方

コードの添削が目的では無いので、本題に入ります。
これは何かたくさんの候補を生成するタイプの問題です。
このような問題ではリストモナドが使えます。
実際に、このプログラムの一行目で `sequence` によってリストがモナドとして用いられています。
でもちょっと遠回りをしているようです。
生成の仕方を変えてみましょう。

この問題の趣旨はこうです。

* 文字のリストが与えられる
* 各文字に対して大文字か小文字かを選択する

これをなるべく直接的にプログラムに落としてみましょう
（次のコードは概念を示したもので、実際のHaskellプログラムとしては正しくありません）。

~~~ {.haskell}
getAllUpperLowerPattern [c1, c2, c3, ...] = do
  d1 <- [toUpper c1, toLower c1]
  d2 <- [toUpper c2, toLower c2]
  d3 <- [toUpper c3, toLower c3]
  ...
  return [d1, d2, d3, ...]
~~~

非決定計算としてのリストモナドの動作から、
各文字が大文字か小文字か、あり得るすべてのパターンが網羅されたリストが返るはずです。

ところで、

~~~ {.haskell}
\[m1, m2, m3, ...] -> do
  v1 <- m1
  v2 <- m2
  v3 <- m3
  ...
  return [v1, v2, v3]
~~~

という関数はまさしく [`sequence`](http://hackage.haskell.org/packages/archive/base/4.5.0.0/doc/html/Prelude.html#v:sequence) です。これを踏まえると `getAllUpperLowerPattern` は

~~~ {.haskell}
getAllUpperLowerPattern str =
  sequence [ [toUpper c, toLower c] | c <- str]
~~~

このようなコードになります（ちなみにこれはすべて違う文字列になるので最後の `nub` は必要ありません）。

ゴルフでもトリッキーなコードでもなく、
計算の仕方を変えるだけでより直接的に、シンプルに、コードが短くなりました。
必要なのはやりたいことが実際にはどのような計算なのかということを認識することです。
難しく考えないようにしましょう。

## 'filesystem-conduit'

おまけで、2つ目の例なのですが、[filesystem-conduit](http://hackage.haskell.org/package/filesystem-conduit) というパッケージに、
ディレクトリのファイル名の `Source` を生成する関数があります。

~~~ {.haskell}
traverse
  :: ResourceIO m 
  => Bool         -- Follow directory symlinks (only used on POSIX platforms)
  -> FilePath     -- Root directory
  -> Source m FilePath
~~~

これを使えば `getTerminals` はこうなります。

~~~ {.haskell}
getTerminals root = runResourceT $ traverse True root $$$$ consume
~~~

`conduit` は現在続々とサポートするライブラリが増えていますので、
ぜひ [Hackage](http://hackage.haskell.org/packages/archive/pkg-list.html) を `conduit` で検索してみてください。
