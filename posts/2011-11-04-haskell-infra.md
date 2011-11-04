---
title: HakyllとpandocとMighttpdでサイトを作った話
description: HakyllとpandocとMighttpdでサイトを作った
tags: haskell
---

## このページについて

ページのフッタに書いてあるので、お気付きの方もいらっしゃるかと思いますが、
このページは
[Hakyll][HKL]と
[Pandoc](http://johnmacfarlane.net/pandoc/)と
[Mighttpd][MIGH]にて運用されています。

完全なソースコードを
<https://github.com/tanakh/tanakh.jp>
から参照できます。

HakyllとはHaskellによる静的ページ生成ツールで、
[Jekyll](http://jekyllrb.com/)にインスパイアされて作られたものだそうです。
JekyllがRubyによって書かれ、Rubyによってページの構造を記述するのに対して、
HakyllはHaskellによって書かれ、Haskellによってページの構造を記述します。

Pandocとはいろいろなドキュメントフォーマットのコンバータです。
Haskellで書かれています。さすがにもうHTMLを手で書きたくはない、
かついろいろなブログの独自のフォーマットなんて覚えていられないので、
汎用的なフォーマットから自分の好みのものを利用できるpandocは非常に便利です。

MighttpdとはHaskellで書かれたHTTPサーバです。
軽量シンプル高性能で、[nginx][NGX]をも越えるパフォーマンスを叩き出します[^1]。

見ての通りHaskellづくしです。
これはHaskellによって作られたインフラによって
十分実用に耐えうるサイトを運用できることを示し、
Haskellが現場で利用するに足る堅牢さと速度を兼ね備えたアプリケーションを
構築できる言語であるということを示すための
実証実験でもあります。
その顛末をここに記しておこうと思います。

## 事の経緯

実のところ最初はこの様な構成にする予定ではなくて、
[github pages](http://pages.github.com/)でサイトを作ろうと思っていました。

このドメイン(<http://tanakh.jp>)をかなり昔に取ってはいたものの、
なかなかまともなコンテンツを作る余裕がなく放置する日々が続きました。
しかし、以前利用していた[isweb](http://isweb.www.infoseek.co.jp/)の無料サービスが終了し、
それに伴い[私のホームページ](http://fxp.infoseek.ne.jp/)も消滅(ついでにデータも消滅)したので、
新しいページを立ち上げようと重い腰をあげたのでした。

github pagesを利用するなら、Jekyllというサイトジェネレータが利用できるということなので、
最初はそれを利用しようと考えました。
ところが、Jekyllを使うのは私にはとても難しかったのです。
Rubyを普段書いていないというのは瑣末なことですが、
とにかく思ったとおりに動作しなかったときに何が原因なのか特定するのがとても困難でした。
オプションの指定もよく分からないし、typoしても何も分からないし、
何ができるかもよく分からない。
普段私がドキュメントとしての型にどれだけ頼りきっているのかと言うことがよく分かりました。

さらに困ったのが環境による動作の違いで、
github pagesにjekyllのファイルを置くと、
github側で実行してサイトを生成して公開してくれるのですが、
これが手元の結果と違ったりして、
そもそも処理系のバージョンもjekyllのバージョンも違うところでリモートで動かして、
しかもRubyとかあまりにも困難なのではないのかと
ようやく私の鈍い頭も理解し始めたわけです。

それで[Hakyll][HKL]です。なんかHaskellに似た様なのがあったなあといううっすらとした
記憶はあったので、どうせならそれを使おうということになりました。
HakyllならバックエンドがpandocなのでさらにHaskell感upです。
せっかくそこまでやるなら、HTTPサーバもHaskell製のMighttpdにして、
Pure Haskellを目指そうかと相成りました。

果たしてこのページの運営がうまくいくのか。
サーバが負荷で死んだりしたらHaskellはまだその域に達していなかったということで。

## Hakyllでサイトを構築する

さて、前置きが長くなりましたが、ようやく本文です。
Hakyllでのサイト構築はとても簡単です。

### Hakyllのインストール

次のコマンドを実行するだけでインストールは完了です。
_cabal_ というのはHaskellのパッケージ管理コマンドで、
[Haskell Platform](http://hackage.haskell.org/platform/) をインストールすれば
同時にインストールされます。

~~~ {.bash}
$ cabal update
$ cabal install hakyll
~~~

Pandocによるシンタックスハイライティングを利用したい場合は

~~~ {.bash}
$ cabal install --reinstall -fhighlighting pandoc
~~~

これを実行してから改めてhakyllをインストールし直します。

### サイトの構築

[チュートリアル](http://jaspervdj.be/hakyll/tutorials.html)と
[サイト例](http://jaspervdj.be/hakyll/examples.html)を見れば
大体どうすればいいのか分かると思います。
[作者自身のサイトのソース](https://github.com/jaspervdj/jaspervdj)が
かなり参考になりました。

~~~ {.haskell}
import Hakyll

main :: IO ()
main = do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  
  ...
~~~

基本的にはどのパスにどういうファイルがあって、
どういうコンパイラで処理するのかを書いていけばよくて、
間違っていたら大体型エラーが教えてくれます。

### ページの記述

hakyllはpandoc組み込みサポートがあり、
Pandocのサポートする任意のフォーマットでページを記述することができます。
現在のところPandocがサポートするのは以下のフォーマットです。

* [markdown](http://daringfireball.net/projects/markdown/)
* [reStructuredText](http://docutils.sourceforge.net/docs/ref/rst/introduction.html)
* [textile](http://redcloth.org/textile)
* [HTML](http://www.w3.org/TR/html40/)
* [LaTeX](http://www.latex-project.org/)

私はmarkdownが好きなのでこのページはmarkdownで書いています。
これらに加えて[Literate Haskell](http://www.haskell.org/haskellwiki/Literate_programming)
のソースもサポートされていますので、lhs+markdownのように
そのままHaskellのコードとして実行できるページなども各個とができます。

また今回は使っていませんが、pandocの出力は、pdfやepubなど
もっとたくさんのフォーマットをサポートしているので、
書籍やドキュメントの作成にも便利です。
[夏に出した同人誌](http://www.paraiso-lang.org/ikmsm/books/c80.html)
はPandocによって各人が好みのフォーマットで原稿を書くことができました。

### hakyll感想

hakyllのスクリプトはroutingはモナドで書き、
ページコンパイラはArrowで処理をつないでいくのが大まかなところです。
型が合えば、大体は変なことにならない安心感が素敵です。
[ドキュメント](http://jaspervdj.be/hakyll/reference/index.html)もしっかりしています。
おかげでライブラリの使い方で迷うところはほとんどありませんでした。

## ページのデザイン

これがとても大変でした。
私の前のページをご存知の方なら分かると思いますが、
デザインセンスは0で、テーブルレイアウトしか書いたことがなかったのでした。

せっかくなのでHTML5+CSS3でモダンなサイトデザインをしようと、
Twitterの[Bootstrap](http://twitter.github.com/bootstrap/)を試したりしましたが、
なんだか全体的にTwitterになってしまったので止めました。

かと言って一から作るのもバッドノウハウ多そうで厳しそうなので
[HTML5 Boilerplate](http://html5boilerplate.com/)を利用することにしました。
ブラウザの互換性などにまつわるいろいろめんどい部分が省略できるようです。

### CSSを書く

テンプレートができたらいよいよデザインなのですが、
[ご覧のとおり](http://tanakh.jp)こんなにしょぼいサイトでも
とても苦労しました。

CSSのデザインは箱を敷き詰めるように行うのだそうですが、
これが全然思ったとおりの位置に収まってくれなくて大変でした。
これもtypoしても全然何も教えてくれないので困りました。
marginを何回もmerginに間違えてその度に脱力感に襲われました。

JavaScriptをどげんかせんといかん、ということでGoogleが
[Dart](http://www.dartlang.org/)というプログラミング言語を発表しましたが、
個人的にはCSSの方を何とかしてほしいと思いました。

## Mighttpdによる公開

さて、コンテンツができたらあとはこれをHTTPサーバで公開するだけですが、
今回は[Mighttpd][MIGH]を使ってみることにしました。

HTTPサーバだとApacheやnginxが有名ですが、
Mighttpdはそれと遜色のない信頼の置ける
動作をしてくれるんじゃないかという期待を込めての選定です。

### Mighttpd紹介

MighttpdはHaskellによって書かれたHTTPサーバです。
ベンチマークによると大変良好なパフォーマンスを示しているようで、
CGIも扱えるようです。
シンプルな分設定も簡単です。

### インストール＆実行

これもcabalからインストールできます。

~~~ {.bash}
$ cabal update
$ cabal install mighttpd2
~~~

実行にはconfファイルとrouteファイルを指定してやります。
~/.cabal/share/mighttpd2-x.y.z/
あたりに設定のサンプルがあるはずなので、それを参考にすれば簡単だと思います。

### Haskellとネットワークプログラミング

Mighttpdは最近のHaskellでのトレンド、
Iteratee[^2] [^3] [^4]
およびそれに基づいたWebアプリケーションインターフェース、WAI[^5]
の上に実装されています。
WAIはWebアプリケーションのフロントエンドとバックエンドを
切り離す働きをしており、Mighttpdの場合はバックエンドに
Warp[^6]という軽量・高速なHTTPサーバが用いられています。

Iterateeとそれに基づく抽象化がなぜ高速性を達成するのかの説明は
他に譲ることにしまして、
ここではWarpについて少し紹介したいと思います。

### 軽量・高速HTTPサーバWarp

HTTPサーバ（ないしはその他のWebサーバ）は並行に動作する必要があります。
なぜならば、サーバはクライアントと通信をして、
そのクライアントとの通信には時間がかかる可能性があるためです。

並行に動作させるために、旧来のHTTPサーバは
リクエストごとにプロセスを起動させていました。
確かにこれは並行に動作しますが、
プロセスを立てるのはかなりのオーバーヘッドです。

当然の改良として、リクエストごとにプロセスではなく、
スレッドを立てるというものがあります。
これはスレッドよりはよいものの、
それでもスレッドを立てるのはまだオーバーヘッドが
小さいとは言えません。
同時に多数のクライアントとのやりとりを行うのも難しいです。
と言うのも、スレッドを一つ立てるごとにある一定のメモリ空間を占有するからです。

そこで、最近は1つのプロセス（スレッド）で多数のリクエストを処理するために
イベント駆動というものがもてはやされています。
これは入出力を非同期で行い、通信の準備が完了したソケットに対して
処理を行うというものです。
実際のアプリケーションでは、
[nginx][NGX]や
[node.js](http://nodejs.org/)といった最近流行りのサーバで利用されているようです。
ソケットを複数監視するために、select()ではなく、epoll()などを用いる
のも特徴で、これにより高速にソケットの監視が行えます。
入出力処理はインターリーブできるものの、
このままでは複数CPUのリソースを活用することはできないため、
実際にはこれをさらにマルチスレッド化するなどされています。

### イベント駆動の問題点

イベント駆動はよいパフォーマンスをもたらしましたが、
プログラミングモデルとしては後退したとも言えます。
問題点は大きく次の二つです。

* すべての処理がブロックしないようにしなければならない

気をつけなければならないというのは、
必ずそれに悩まされるということでもあります。
node.jsがJavaScriptを選択したのも、
JavaScriptにはもともとブロッキングI/Oが存在せず、
この問題も発生しなかったからともいわれます。

* コードがスパゲッティになる

I/O完了後の処理を分けて書く必要があり、
コードの見通しが悪くなります。
どちらかというとこちらが深刻な問題です。

### 軽量スレッドと高性能I/Oスケジューラ

マルチスレッドの利便性を保ったまま、
ハイパフォーマンスを達成できないものでしょうか？

できるんです。Haskellなら。

Haskellのスレッドはネイティブスレッドではなく、
処理系の作るスレッドです。
Haskellのスレッドは非常に軽量で、1つあたり数百バイトしか消費しません[^7]。
コンテクストスイッチのオーバーヘッドもカーネルに入らないので非常に軽微です。
スレッドの実行は適切にスケジュールされてネイティブのスレッドに割り振られ、
CPUリソースを有効に利用することができます。

HaskellのI/Oはすべて非同期で行われます。
時間のかかる可能性のあるI/Oを行ったスレッドは自動的に
I/Oスケジューラで管理されるようになり、
入出力の準備が完了したものから再度実行キューに投入されます。

ここでGHC6.12以前ではselect()が用いられていて、
パフォーマンスが出なかったり、
select()の制約から、1024個以上のソケットを
扱えなかったりしていたのですが、
GHC7.0以降で実装されたスケーラブルI/Oマネージャ[^8]で
epoll()を用いた実装に代わり、
その問題は解決されました。

### そして、Warpへ

Warpはこれらのインフラの上に構築された、
簡潔で高速なHTTPサーバというわけです。
Iteratee、さらに最近の高速テキストライブラリ、
パージングライブラリ、その他諸々の
最新コンセプトがてんこ盛りで、
これらが研究対象だけでなく、
実際に高速で信頼性の高いアプリケーションを構築するのに役立つということを
実証するショーケースとしての役割も果たしていると言えます。

### 最後に、Mighttpd

この頑強なバックエンドの上に構築されているのが
まさしく今このブログが動作しているMighttpdと言うわけなのです。
どうでしょうか。Mighttpdに興味を持ってもらえましたか？

## まとめ

Haskellによるインフラを用いて、上から下までサイトを構築してみて、
Haskellは何にでも使えるという感触が確かなものになりました。

Scalaは小さい書き捨てプログラムから大規模プログラムにまで
すべてに適しているとして自らをScalableプログラミング言語と称しました。

Haskellもまさにそれであると思います。
テキスト処理からハイパフォーマンスサーバまで、
なんでもこなせる可能性を秘めています。
そして、最近のライブラリの充実により、
その可能性が実際に発露してきているといえるでしょう。

Haskellはpurelyだとかlazyだとか、そういったところが殊更に取り上げられて、
これはマーケティングの失敗何じゃないかと思います。
purelyだからI/Oが苦手というのはまったくの誤解で、
実際には最も美しい命令型言語[^9]という評もあるぐらいなのです。

さて長くなりましたが、Haskellが実際に使える言語なのだということを
示す1つの証拠として当ブログが存在していければよいなあと
思いつつ、ここらで今回は終わりにしたいと思います。

[MIGH]: http://www.mew.org/~kazu/proj/mighttpd/en/
[NGX]: http://nginx.org/
[HKL]: http://jaspervdj.be/hakyll/

[^1]: <http://www.mew.org/~kazu/material/2011-mighttpd2.pdf>
[^2]: <http://www.haskell.org/haskellwiki/Iteratee_I/O>
[^3]: <http://www.haskell.org/haskellwiki/Enumerator_and_iteratee>
[^4]: <http://research.preferred.jp/2011/02/enumerator-package-yet-another-iteratee-tutorial/>
[^5]: <http://hackage.haskell.org/package/wai-0.4.2>
[^6]: <http://steve.vinoski.net/pdf/IC-Warp_a_Haskell_Web_Server.pdf>
[^7]: <http://www.slideshare.net/nushio/peyton-jones2011parallel-haskellthefuture>
[^8]: <http://static.googleusercontent.com/external_content/untrusted_dlcp/research.google.com/ja//pubs/archive/36841.pdf>
[^9]: <http://research.microsoft.com/pubs/74063/beautiful.pdf>
