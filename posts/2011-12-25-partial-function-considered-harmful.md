---
title: Partial Function Considered Harmful
description: 部分関数は有害である
tags: programming, Haskell, considered harmful
---

この記事は、[Haskell Advent Calendar 2011](http://partake.in/events/eaea52c2-61ef-46d5-a855-3a2dde459e3a)
25日目の記事として書かれました。

## 概要

Haskell、あるいはその他のプログラミング言語では
「部分関数(Partial Function)」
と呼ばれるものが標準ライブラリに存在したり、
定義したりすることができます。
今回はそれらが有害であるという考えと、
代替の紹介をしようと思います。

## 部分関数とは

部分関数(Partial Function)とは、
集合の言葉で言うと、

> 定義域(domain)の要素に対して、値域(range)の値が高々1つ対応付けられる

ような対応付けのことです。
Haskellでは
「結果の値が引数によっては定義されないことがあり得る」
関数だと言えます。

例えば、整数の割り算を行う関数 `div :: (Int, Int) -> Int` は、
`(1, 0)` に対しては定義されません。

Haskellでは部分関数を定義するのに、幾つかの方法があります。
1つ目は、定義されないことを明示するために、`undefined`を用いるものです
(コードがuncurry化されていることには特に意味はありません)。

~~~ {.haskell}
div' :: (Int, Int) -> Int
div' (a, b)
  | b /= 0 = a `div` b
  | otherwise = undefined
~~~

~~~ {.haskell}
ghci> div' (1, 0)
*** Exception: Prelude.undefined
~~~

`undefind` の代わりに、`error` や `throw` を用いることも出来ます。

もう1つはパターンマッチを網羅しないものです。

~~~ {.haskell}
data Hoge = Foo | Bar

test :: Hoge -> Int
test Foo = 0
~~~

~~~ {.haskell}
ghci> test Bar
*** Exception: test.hs:4:1-12: Non-exhaustive patterns in function Main.test
~~~

対応するパターンが無い旨がエラーとして表示されます。

## 部分関数はなぜ良くないのか？

部分関数が良くない理由はただ1つ、

***部分関数での未定義をエラーとして補足することが非常に困難***

だからです。

実際に XMonad[^xmonad] のようなクラッシュすることが許されない類のプロダクトでは、
部分関数を排除するスタイルのコーディングをルールにしているものもあります[^xmonad-style]。

## なぜ困難なのか？

部分関数の未定義を捕捉するのが困難なのには幾つかの理由があります。

* 型として現れない

ある関数が部分関数なのか、あるいはそうでないのか、
Haskellでは型の部分に現れません。
つまり部分関数のエラーの捕捉に関しては、
型システムのチェックを利用できないということです。

* Haskellでの未定義の扱い

Haskellでの未定義は実際には例外によって実現されています。
Haskellでは、`throw`関数によってpureなコードから
例外を投げることができます。

pureなコードで例外を投げることの意味についてはここでは
詳しく述べませんが、少なくとも幾つかの難しい部分があります。

1つ目は、例外を拾うことができるのはIOモナドの中だけだということ。
2つ目は、pureなコンテクスト下では、例外の発生も遅延されるということ。

簡単な例を見てみましょう。

~~~ {.haskell}
getNums :: IO [Int]
getNums = do
  line <- getLine
  return $ map read $ words lines

main :: IO ()
main = do
  nums <- getNums `catch` \(SomeException _) -> return []
  print $ sum nums
~~~

`getNums` は標準入力から1行入力してスペース区切りの整数のリストを読み込みます。
整数として正しくない文字列が入力された場合、例外が発生します。
`main` では `getNums` を呼び出し、受け取った数の合計を表示します。
getNumsが例外を発生させた場合は、空リストを受け取ったことにします。
つまり不正な入力が来た場合、表示されるものは0になるはずです。

~~~ {.haskell}
$ runhaskell Test.hs
1 2 3
6
$ runhaskell Test.hs
hello, world!
Test.hs: Prelude.read: no parse
~~~

1つ目の実行結果は正しく動いているようです。
問題は2つ目です。さて、望ましくない挙動です。
どうしてこうなるのか。これはまさしく例外の発生が遅延されているからです。
`getNums` 呼び出しの時点では例外は発生せず、
実際にその値が必要とされるところ、この場合だと
`print $ sum nums` で発生します。
つまり発生場所に `catch` を記述して例外を捕捉しようとしてもそれをするりとすり抜け、
全く別の場所で例外を起こしてしまうのです。
実際に値がどこで使われるのか、
はたまた今使っているものが例外を起こしうるのか？
例外をpureに投げることのできるHaskellでは知るすべはありません。

これを何とかするためには、
例外を拾う場所で評価を行い、
例外を起こさせるようにする方法があります。
`Control.Exception` モジュールにそのための関数、 [`evaluate`](http://hackage.haskell.org/packages/archive/base/4.4.1.0/doc/html/Control-Exception.html#v:evaluate) という関数があります。

~~~ {.haskell}
main :: IO ()
main = do
  nums <- do
    n <- getNums
    evaluate n
    `catch` \(SomeException _) -> return []
  print $ sum nums
~~~

しかし、残念ながらこれもうまくは動きません。
というのも、evaluateは[WHNF(Weak Head Normal Form, 弱頭部正規形)](http://en.wikibooks.org/wiki/Haskell/Graph_reduction#Weak_Head_Normal_Form)までしか評価しない、つまりリストの中身までは評価しないからです。
なぜそのような動作になっているのかというと、Haskellの標準的な機能では
データ構造をWHNFまでしか簡約できないからです。

[deepseqパッケージ](http://hackage.haskell.org/package/deepseq)にある
[NFDataクラス](http://hackage.haskell.org/packages/archive/deepseq/1.2.0.1/doc/html/Control-DeepSeq.html#t:NFData)は、
NF（Normal Form, 正規形）までの簡約を行うインターフェースを提供しています。
これを利用することによって、先程の例は正しく動作するようになります。

~~~ {.haskell}
main :: IO ()
main = do
  nums <- do
    n <- getNums
    return $!! n
    `catch` \(SomeException _) -> return []
  print $ sum nums
~~~

~~~ {.bash}
$ runhaskell Test.hs
hello, world!
0
~~~

しかし、プログラムのすべての場所についてこのような注意を払うのは大変困難ですし、
すべての評価したい値が `NFData` クラスのインスタンスにできるとも限りません。
その最たるものが関数です。

* 出力がプアー

エラー時のメッセージが、先程の例の

> Test.hs: Prelude.read: no parse

のように、非常に質素なものです。
これに加え、Haskellではスタックトレース、
すなわちこの例外が生成された場所を知る術がありません。
それは、実際には例外が作られた場所と評価された場所が異なることに起因するもので、
今のところ簡単な解決方法はありません。
大きなプログラムで突然何の情報もなくエラーで落ちる。
そしてどこが原因か全くわからない。
デバッグは非常に困難です。

ところでこれはいろいろ問題だと認識されていて、
Haskellにスタックトレースを実装して
もうすこしまともな情報を得られるようにしようという試みは
いろいろなされていて[^stacktrace]、
近い将来GHCに実装されるのではないかと思われます。

## 身の回りの部分関数

エラーとして部分関数を用いている関数が、
身近なところにも実はとてもたくさんあります。

Preludeだけでも、

* head / tail / init / last
* foldl1 / foldr1 / maximum / minimum
* Enumクラスのメソッド全般
* read

などなど、非常によく使うものが含まれています。
Preludeは一歩間違えれば地雷に足を突っ込んでしまう
超危険地帯なのです。

## 部分関数ではないもの

一方、エラーをIOモナドの例外として送出するものは
部分関数ではありません。

Haskellの例外には `throw` によってpureに送出されるものと
`throwIO`によってIOモナドとして送出されるものがありますが、
`throwIO`の方は、IOモナドのコンテクストにて正しくハンドリングされますので、
部分関数ではありません。

例えば、`getLine` は `IO String` の型を持ちますが、
これはIOのエラーとして例外が投げられます。

## 部分関数の除去

さて、あなたのプログラムに潜む地雷を撤去したくなったでしょうか？
そのための幾つかのオプションがあります。

### 既存の部分関数を使うのを避ける

先ほど挙げたPreludeの関数、及び他の標準的な部分関数を使うのを避けましょう。
ある関数が部分関数かどうか見分けるのは、型から判断することはできませんので
ドキュメントに頼ることになるでしょう。
しかし逆の、部分関数ではない関数を見分ける方法ならいくつかあります。

* IOモナドである

`throwIO` によってエラーが通知される可能性が高いです

* 返り値が`Maybe`あるいは`Either`である

あえて部分関数にしている可能性は非常に低いです。

* その他何らかのエラーモナドになっている

[`MonadError`](http://hackage.haskell.org/packages/archive/mtl/2.0.1.0/doc/html/Control-Monad-Error-Class.html#t:MonadError)
あるいは
[`Failure`](http://hackage.haskell.org/packages/archive/failure/0.1.0.1/doc/html/Control-Failure.html#t:Failure)
など、どういうエラーでどういうモナドか明示されている場合、
それによってエラーが通知される可能性が非常に高いです。

このようなエラーの通知手段が明示されている関数を好んで利用するのは
良い習慣です。

標準ライブラリの部分関数の利用を避けるため、
それらの非部分関数（total function、もしくは単に関数）版が用意されているパッケージ、
例えば [safeパッケージ](http://hackage.haskell.org/packages/archive/safe/0.3.3/doc/html/Safe.html) のようなものがあります。
こういったものを用いるのも良い代替でしょう。

また標準ライブラリの部分関数には、
IOモナド版が用意されているものもあります。
例えば `read :: Read a => String -> a`
に対して `readIO :: Read a => String -> IO a` というものがあります。
返り値にIOが付いただけですが、これが非常に重要なことです。
というのも、IOが付くことによって正しいエラー伝達手段が用意されたということ
になるからです。`readIO` を用いると、
先の例はとてもシンプルに書きなおすことができます。

~~~ {.haskell}
getNums :: IO [Int]
getNums = do
  line <- getLine
  mapM readIO $ words lines

main :: IO ()
main = do
  nums <- getNums `catch` \(SomeException _) -> return []
  print $ sum nums
~~~

~~~ {.haskell}
$ runhaskell Test.hs
hoge--- moge---
0
~~~

このようにIO版が用意されている場合、
それを用いるのは有力な選択肢となります。

### 部分関数を作るのを避ける

部分関数を利用するのを避けたなら、
作るのも避けなければなりません。

* パターンマッチ漏れを防ぐ

とても単純なことですが、
パターンマッチ漏れを防ぐことは重要です。
パターンマッチに漏れがあるかどうかは
GHCの場合コンパイラが自動でチェックして
警告を出してくれるので、
それらを無視せずにきちんと修正しましょう。

あるいはパターンマッチを使わずに済むなら、
なるべく使わないようにするというのも
考慮すべきオプションでしょう。

* 部分関数にする代わりにIOモナドにする

`throw`、`error` そして `undefined` は部分関数を構築するプリミティブです。
これらを使わないようにしましょう。
単純にそれらを `throwIO` で置き換えれば問題は解決です。

* 部分関数にする代わりに `Maybe`や`Either` あるいはその他のエラー伝達手段を用いる

もともとIOモナドの一部なら`throwIO`を用いるのに特に問題はありませんが、
純粋な計算ではIOが混入することは嫌われるかもしれません。
その場合には`Maybe`や`Either`を使うのが良いでしょう。
または `MonadError`あるいは`Failure`で
ポリモーフィックにエラー通知をするのがスマートです。

## 遅延IOの除去

さて最後にもう一つ、厄介なものを紹介します。
部分関数ではないものの、部分関数と同様に扱いにくいもの、遅延IOです。
遅延IOでよく用いられるものに `getContents` などがあります。
`getContents`は標準入力からの遅延入力を行う関数です。
入力は必要とされるまで実際には行われません。
実際に入力が行われるときにエラーが発生すると、
その入力を処理していたpureなコンテクストで例外が発生します。
部分関数と同様にこれは非常に捕捉しにくいものになります。

あるいは、エラーを起こす可能性のあるIOモナドを
`unsafeInterleaveIO`または`unsafePerformaIO`することによっても
同様の現象が発生します。

（結局のところ、pureなコンテクストで例外を発生させるということが良くないのです）

[*Lazy I/O Considered Harmful*](http://sites.google.com/site/haskell/notes/lazy-io-considered-harmful-way-to-go-left-fold-enumerator)
なる記事も書かれており、エラーハンドリングの困難さはその問題の１つとして挙げられています。

しかし困ったことに遅延IOというものは非常に便利な側面もあり、
堅牢なコードを書くためにこれを避けるのはもったいないものです。

そこで、遅延IOを避けつつ、遅延IOがもたらす利益を享受しようと設計されたのが
いわゆる `Iteratee`(もしくは`Enumerator`) と呼ばれるものです。

`Iteraete` に関しては、日本語を含むたくさんの解説記事 [^iter0] [^iter1] [^iter2] [^iter3] がありますので、
ぜひそちらをご参照ください。

`Iteratee` 周りの動きは非常に活発で、
すでに多くのライブラリがこのインターフェースに基づいた実装を行ったり、
最近になって [Conduit](https://github.com/snoyberg/conduit)という
設計を見なおして使いやすくした代替ライブラリなどが出現しています。

## まとめ

部分関数を用いるのは堅牢なプログラムを書く上で望ましくないという
話をしてきました。
実際のところ、エラーの発生する関数を部分関数にしてしまうのはとてもお手軽なので、
書き捨てのプログラム等では無理に除去する必要はないでしょう。
しかし、長時間動作する必要のあるサーバのようなプログラム、
あるいは汎用的に用いられるライブラリなどは
必ず代替手段を用意しておくべきです。

堅牢なプログラムのためには、
部分関数を除去し正しくエラーを通知して、
それから正しくエラーを処理する必要があります。
そのために、[monad-control](http://hackage.haskell.org/package/monad-control)というパッケージが非常に有用なのですが、それは又の機会に。

あなたのHaskellプログラムが正しく動き続けますように。
Happy Haskelling and Happy Holidays!

[^xmonad]: Haskellで書かれたタイル型Window Manager <http://xmonad.org/>
[^xmonad-style]: <http://code.haskell.org/xmonad/STYLE>
[^stacktrace]: <http://community.haskell.org/~simonmar/slides/HIW11.pdf>

[^iter0]: <http://themonadreader.files.wordpress.com/2010/05/issue16.pdf>
[^iter1]: <http://www.yesodweb.com/book/enumerator>
[^iter2]: <http://research.preferred.jp/2011/02/enumerator-package-yet-another-iteratee-tutorial/>
[^iter3]: <http://d.hatena.ne.jp/kazu-yamamoto/20110216/1297845048>
