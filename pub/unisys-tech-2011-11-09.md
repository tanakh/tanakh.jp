% Haskellの楽しみ：<br>手続き操作型というパラダイム
% 田中英行 <tanakh@preferred.jp>
% 2011/11/09 技術交流会 @日本ユニシス

### 自己紹介

<div style="position:absolute; top:375px; left:650px;">
![LYAHFGG](http://nostarch.com/sites/default/files/imagecache/product_main_page/lyah.png)
</div>

* 田中英行 (@tanakh)
* (株)Preferred Infrastracture 研究開発部門
    * 社内ライブラリ _pficommon_ <br>
      <https://github.com/pfi/pficommon>
    * 分散機械学習フレームワーク _jubatus_ <br>
      <http://jubat.us/>

* Haskell愛好家
    * 2003～

* Learn You a Haskell for Great Good!
    * 訳してます

### 本日の概要

<br>

・ Haskellは **簡単**

・ Haskellは **すごい**

・ Haskellは **面白い**

<br>

<div class="build">
・ Haskell は簡単ですごく面白い

・ Haskell は面白くてすごいものが簡単に作れる

<br>（あとモナドの話少し）

</div>

## 最近のHaskellの話題

### Yesod

* <http://www.yesodweb.com/>
* Webフレームワーク

* 速い！ 安全！ 使いやすい！

![YESOD](http://www.yesodweb.com/static/custom/header.png)

### scalable-server

* <http://hackage.haskell.org/package/scalable-server>
* スケーラブルなサーバを超簡単に書けるライブラリ
    + I/O 全て隠蔽
    + 非同期隠蔽
    + 軽量スレッド
* [attoparsec] + [enumerator] + [blaze-builder] + [text] +<br>
  GHC's Scalable I/O manager

<div align="center">
**no more event-driven!**
</div>

### angel

* Haskellによる[daemontools]の再実装

* [bump](http://bu.mp/) というサービスの運用に利用されているらしい
    * 世界で1千万以上のユーザ + 1年以上無停止

![BUMP](http://bu.mp/static/images/handswithdrawings.png)

### pandoc

* 汎用ドキュメントコンバータ

* HTML, LaTex, Markdown, reST, PDF, epub, ...
* 非常にシンプルなドキュメント型

<br>

* このプレゼンもpandocで作られています
    * markdown => Pandoc => HTML5 Slide
    * [ソース](./unisys-tech-2011-11-09.md)ご参照

### attoparsec

* 高速・シンプルなパーザライブラリ

### acid-state

* ACIDを保証するデータ永続化ライブラリ

### shakespeare

* Hamletなど準クオート群

~~~ {.haskell}
main :: IO ()
main =
    writeFile def "test3.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "html" [] [xml|
<head>
    <title>
        My #
        <b>Title
<body>
    <p>foo bar baz
|]
~~~

### Configurator

* 設定ファイル読み込みライブラリ
    * 自動変更監視
    * 変更通知

## Haskellの楽しみ

### よくある誤解

* Haskellは **難しい**
* 難しいことが簡単に
  簡単なことが **難しく**
* I/O が **苦手**
* 強い型付けは **面倒**

### 今日は

* 遅延評価
* 参照透明

のことは一旦忘れてください

<br>

ことさらに取りざたされますが、これはあまり重要じゃない（と思います）です

### Haskellは **難しくない**

* C++

~~~ {.cpp}
#include <iostream>
int main()
{
  std::cout << "Hello, World!" << std::endl;
  return 0;
}
~~~

### Haskellは **難しくない**

* Java

~~~ {.java}
public class Main {
  public static void main(String[] args) {
    System.out.println("Hello, World!");
  }
}
~~~

### Haskellは **難しくない**

* Haskell

~~~ {.haskell}
main = putStrLn "Hello, World!"
~~~

### I/Oが苦手？

* IOモナドは扱いにくいか？
    * そんなことはない

* 記述上のデメリットはもう殆ど無いといえる
    * do 構文が便利
    * Aplicativeスタイルが便利

* IO処理を値として使える
    * その他もろもろのモナド変換子、モナドリフタ…
    * むしろ他の言語よりIOが柔軟に行える
    * SimonPJ曰く、"Haskellはもっとも美しい手続き型言語"

### なんとなくそういう印象を受ける

* 参照透明な言語でコードを書いたことがない
* モナドを利用したコードを書いたことがない

経験の問題！まずはコードを書いてみましょう

### 型付けは安心のため

* 型はエラーを防ぐ
* 型は良いドキュメントになる

⇒ 初めて使うライブラリでも安心して使える

### 型は面倒ではない

* Hindley-Milner型推論
    * <http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner>
* 大部分の型を処理系が自力で見つける
    * 変数
    * 関数の引数
    * 型クラス…

~~~ {.haskell}
-- foo :: Num a => a -> a
foo n = n + 1
~~~

### 型は役に立たない？

Java 1.4

~~~ {.java}
class Hoge {
  public static void main(String[] args) {
    List l;
    l.add("string");
    Integet n = (Integer)l.indexOf(0); // Oops...
  }
}
~~~

### 型は役に立たない？

Java 1.5

~~~ {.java}
class Hoge {
  public static void main(String[] args) {
    List<String> l;
    l.add("string");
    Integet n = l.indexOf(0); // Compile Error :)
  }
}
~~~

型システムの有用性は型システムの表現力に左右される

### Haskellの型システム

* Haskellの型システムは、広く利用可能な言語の中で、最も強力・かつ扱いやすい部類

* 型システムのいろいろは Lambda Cube とかで熟知すべし
    * <http://en.wikipedia.org/wiki/Lambda_cube>

* [Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/) などの書籍も

### Haskellの型に特徴的な機能

* Type Class
* Rank-N-Polymorphism
* GADTs
* Type Families

とにかく強力！

## モナド：手続き操作型というパラダイム

### モナドに関する疑問

* モナドとは何か？
* モナドをどう使うか？
* モナドをどう作るか？
* *なぜ* モナドなのか？

### こちらもご参照ください

<div style="width:780px" id="__ss_9290002"><iframe src="http://www.slideshare.net/slideshow/embed_code/9290002" width="425" height="355" frameborder="0" marginwidth="0" marginheight="0" scrolling="no"></iframe> <div style="padding:5px 0 12px"> View more <a href="http://www.slideshare.net/" target="_blank">presentations</a> from <a href="http://www.slideshare.net/tanakh" target="_blank">tanakh</a> </div> </div>

### モナドとはどういうものか？

* 設計手法の１つ？
* デザインパターンの１つ？

### モナドはパラダイムである

* 応用範囲が広い
    * 濫用じゃないのかと思った時期もありました

* 特定の問題を解決するものではない
    * 何にでも使えるという事実（後述）

* ⇒ **パラダイム** である！
    * オブジェクト指向のオブジェクトのように、まずモナドありきで考えるべきもの

### モナドとは何か？

* 計算のコンテクスト
    * それがどう言う計算を表すか
    * 床下配線のアナロジー

* モナド＝コンテクストの総称
    * コンテクストの抽象化ができる

* ⇒ コンテクストに対する抽象化を行うのが、まずひとつのモナドを使う理由

### 例

Intを返す **何らか** の計算

~~~ {.haskell}
foo :: Monad m => m Int
foo = return 1
~~~

Intの状態を持ちBoolを返す **何らか** の計算

~~~ {.haskell}
bar :: MonadState Int m => m Bool
bar = do
  state <- get
  return $ state == 0
~~~

(コンテクストに対してポリモーフィックなことに注意)

### モナドのコンテクストの具体例

* 非決定計算
    * List
* 例外処理・エラー処理
    * Maybe, Either, MonadError, MonadFailure
* 継続・限定継続
    * MonadCont
* メモ化
    * MonadMemo
* 並行・並列計算
    * Parモナド、STMモナドなど
* 分散計算
    * CloudHaskell

### なぜ、モナドなのか？

* モナドで充分だから

* モナドはチューリング完全である
    * 構造化定理の3つの構造が記述できる

* 構造化定理
    * 逐次： ２つの処理を順番に実行する
    * 反復： 条件の満たされる間処理を繰り返す
    * 分岐： 条件により分岐する

### モナドで十分な理由

* あちら側（モナド）とこちら側（ホスト言語：Haskell）をシームレスに繋げる

~~~ {.haskell}
main = do
  line <- getLine   -- get 'Monads value'
  if line == ""     -- depend of its value...
    then return ()
    else do         -- changes the built monad
      putStrLn line
      main
~~~

* これはまさにメタプログラミングを実行時の情報を用いて行っている
    * メタメタプログラミングとでも言うべきもの

### モナドから見た命令型

* 計算コンテクストの1つに過ぎない

* その他：
    * 論理型
    * オブジェクト指向
    * アスペクト, etc…

<br>

* これらはモナドのコンテクストとして実装できる

### モナドはパラダイムを抽象化するもの

* では、モナドはなんなのか？

    * ⇒ メタパラダイム

* IOモナド（手続き型コンテクスト）を用いるHaskell

    * ⇒ **手続き操作型パラダイム**

<br>
（という命名を思いつきました）

### IO モナドの正体

* 所謂Cなどの命令型言語のコンテクスト一式のパック
    * 逐次実行
    * メモリの読み書き
    * 入出力
* HaskellがCでできることの全てかつCとのリンクをできることを保証するためのFallback

### MonadIO

* そこのコンテクストに、IOを埋め込めることを保証
    * つまりどこでもなんでもできることを保証

* IOモナドを避けつつ、どこでもprintfデバッグできるので便利

### MonadControlIO

* IOにそのコンテクストを渡す事ができるのを保証
    * _bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c_ とか

* 安全な例外処理を実装するために必要

### Identityモナド

* 何のコンテクストも持たない（追加しない）モナド
* Haskellのセマンティクスそのもの

### モナドを扱う言語で望ましいこと

* Identityモナドがなるべくプレーンなコンテクストであるのが望ましい
    * コンテクストの追加はできても除去はできないため

* 遅延評価
    * モナドの評価は遅延されなければならない（後述）

* 一方、純粋関数型言語
    * 副作用なし
    * 遅延評価

### モナドと遅延評価

モナドが再帰している例：

~~~ {.haskell}
rep p = p >> rep p <|> mempty
~~~

遅延評価じゃなければ…

~~~ {.haskell}
rep p = (\_ -> p >> rep p () <|> mempty)
~~~

一段クロージャに包む必要がある<br>
（これでも遅延評価と同じとは行かない）

### モナドでのプログラミング

* まず、やりたい計算がどういうものか考える
    * 具体的にどういう計算かを考えるのではなくて、どういうコンテクストを持つのか
    * 状態を持つ、並列制御がいる、エラー処理が必要、etc...
    * この時に全てのことを考える必要はない
    * モナド変換子で後からコンテクストを足すことができる

* それからモナドを作る
    * 床下の配線を書く
    * 抽象化できるならモナドクラスにする

* それからアプリを書く
    * 大抵のものはありあわせのモナドの組み合わせでなんとかなるので、ここからスタートできる

### まとめ

* Haskellコミュニティは急速に活性化している
* 高速・堅牢・安全・簡潔 を支えるインフラが充実
* モナドの使われ方も成熟してきた
* ⇒ Haskellは現場言語へ！

![HS](http://t2.gstatic.com/images?q=tbn:ANd9GcSr6r9kjS15NKCkZDQopMwDUYkxGvBYh55XzlOvJjyRRY63DMJP)

<div align="center">
***Happy Haskelling!***
</div>

[attoparsec]: http://hackage.haskell.org/package/attoparsec
[enumerator]: http://hackage.haskell.org/package/enumerator
[blaze-builder]: http://hackage.haskell.org/package/blaze-builder
[text]: http://hackage.haskell.org/package/text
[daemontools]: http://cr.yp.to/daemontools.html
