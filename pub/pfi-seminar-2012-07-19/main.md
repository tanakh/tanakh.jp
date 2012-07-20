% たのしいHaskellのツールチェインとC++
% 田中英行 <tanakh@preferred.jp>
% PFIセミナー 2012/07/19

## 自己紹介

* 田中英行, [@tanakh](http://twitter.com/tanakh), <http://tanakh.jp>

* PFIで働くプログラマ

* Haskell愛好家

    * 拙訳「すごいHaskellたのしく学ぼう! 」発売中

          <iframe src="http://rcm-jp.amazon.co.jp/e/cm?lt1=_blank&bc1=000000&IS2=1&bg1=FFFFFF&fc1=000000&lc1=0000FF&t=peropero0b-22&o=9&p=8&l=as4&m=amazon&f=ifr&ref=ss_til&asins=4274068854" style="width:120px;height:240px;" scrolling="no" marginwidth="0" marginheight="0" frameborder="0"></iframe>

## 本日の概要

* ソフトウェアテストを取り巻く環境

* C/C++のコードをQuickCheckする

* C/C++のコードをCriterionでパフォーマンス測定する

# テスト駆動開発

## Not enough!

![ne](ne.png)

<div class="source">
    <http://blog.fogus.me/2012/06/20/not-enough/>
<div>

---

![notenough](notenough.png)

<div class="source">
    <http://evanfarrer.blogspot.ca/2012/06/unit-testing-isnt-enough-you-need.html>
<div>


## Static typing is not enough!

![stine](stine.png)

<div class="source">
    <http://blog.fogus.me/2012/06/20/not-enough/>
<div>

## Contracts are not enough!

![cane](cane.png)

<div class="source">
    <http://blog.fogus.me/2012/06/20/not-enough/>
<div>

## XX are not enough!

![nes](nes.png)

（オチがそれかよ…）

## 絶対にバグのないプログラムを書く方法

![pdd](pdd.png)

<div class="source">
    <http://www.iij-ii.co.jp/lab/techdoc/coqt/>
<div>


## 一般的な話

* [テスト](http://en.wikipedia.org/wiki/Software_testing)はプログラムの正しさを **保証しない**
* [契約](http://en.wikipedia.org/wiki/Design_by_contract)（Contract）はプログラムの正しさを **保証しない**

* [型](http://en.wikipedia.org/wiki/Type_system)はプログラムの正しさを **保証しない** が、
    * ある種の性質は保証できる

<br>

* [証明](http://en.wikipedia.org/wiki/Interactive_theorem_proving)はプログラムの正しさを **証明する**

## 定理証明系

* 定理証明 ⇒ Fully Dependent Typedなプログラミング言語でプログラムを書く

    * [Coq](http://coq.inria.fr/) / [Agda2](http://wiki.portal.chalmers.se/agda/pmwiki.php)

* 実は、ある型のプログラムを書くことは、ある命題の証明と対応する！

    * [カリー＝ハワード同型対応](http://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)
    * 「型↔命題」「プログラム↔証明」

![Curry](http://aprendehaskell.es/_images/curry.png)

## なぜコードの正しさを証明しないのか？

* 定理証明の付いたコードを書くのは大変

    * そもそもむずい
    
        * 普通に書くのはとても難しいので、tacticを用いて、
          ゴールを分割し、穴埋めしながら書く
    
    * コードがでかくなる
    
        * 証明なしのコードの10倍以上になるとか
        * [COMPCERT](http://compcert.inria.fr/) (Coqで証明されたCコンパイラ作るプロジェクト)

<small>（こんなことを書きつつ、私はあまり詳しくないです…すみません）</small>

## コードの品質のために、我々は何をするべきか？

* コードの正しさを保証することと、そのコストはトレードオフ

* 何をやって何をやらざるかは単に開発コストの問題

    * これは情報学の問題でも数学の問題でもない

* スイートスポットを攻めるべき

    * 小さいコストで大きなリターンを得られるものから採用する

## In my opinion...

* ユニットテスト

    * やらないよりはやった方がいい

* 静的型付け

    * 効果大
    * テストは型の代用ではない

* 証明

    * 無理 (´>_>`)

## テストに「ちょい足し」

* Generative Test

    * テストケースを乱数で自動生成
    * 普通のユニットテストを書くコストと大差がない
    * コストをはるかに上回る恩恵！

ぜひ Generative Test をやりましょう！

## だけど…

* 利用できる言語が限られる

    * [QuickCheck](http://hackage.haskell.org/package/QuickCheck) が有名
        * Haskell, Erlang
        * 実際よく使われてる
    
    * 他の言語にも類似の実装が割りとあるが、完成度はまちまち
        * [C++実装](http://software.legiasoft.com/quickcheck/) もあるが…
        * バージョン0.02 ... イマイチ感メンテナンスされてない感 ...

## そういう事なら

<pre>
　 　　　　 |
　 　＼　　__　　／
　 　＿　（ｍ）　＿ﾋﾟｺｰﾝ
　 　　　　|ミ|
　 　 ／ 　｀´　 ＼
　　　　　(´･_･`) HaskellのQuickCheckでC++コードを
　　　　　ノヽノヽ テストすればいいじゃまいか！？
　　　　　　　くく
</pre>


# QuickCheck

## QuickCheckについて

* <http://www.cse.chalmers.se/~rjmh/QuickCheck/>

    * ICFP 2000 初出（？）
    * ICFP 2010、「最も世の中に影響を与えた10年前の論文」
        * （これが最も影響あったということに、関数プログラミングの影響力の小ささが窺い知れる…）
    * 強力な型システムとあわせて、Haskellコードの品質向上に
      実際大きく貢献している

## Quick start of QuickCheck

1. [Haskell Platform](http://hackage.haskell.org/platform/) をインストール

2. テストを含む cabal プロジェクトを作る

3. テストを書く

4. テストを実行

5. 繰り返し

## Haskell Platform インストール

ダウンロードして、マウスをホチポチすれば入る（Linux以外）
![hp](hp.png)

## cabal プロジェクトを作る

`> cabal init` とタイプして、適当に質問に答える

~~~ {.bash}
PS C:\Users\tanakh\Dropbox\project\tmp\qc-test> cabal init
Package name? [default: qc-test]
Package version? [default: 0.1.0.0]
Please choose a license:
 * 1) (none)
   2) GPL-2
   3) GPL-3
...
What does the package build:
   1) Library
   2) Executable
Your choice? 2
Include documentation on what each field means (y/n)? [default: n]
Guessing dependencies...
...
Generating qc-test.cabal...
~~~

## プロジェクトにテストを追加

`<プロジェクト名>.cabal` というファイルが生成されたはずなので、それを編集

~~~ {.haskell}
name:                qc-test
version:             0.1.0.0
-- synopsis:            
-- description:         
license:             BSD3
（略…）
executable qc-test
  main-is:             main.hs
  build-depends:       base ==4.5.*
-- ↓これを追加
test-suite hoge-test
  type:                exitcode-stdio-1.0
  main-is:             test.hs
  build-depends:       base  ==4.5.*
                     , hspec >=1.3
~~~

## なんかプログラム書く

* 今回はリストの足し算をする関数を書いてみます

~~~ {.haskell}
mySum :: [Int] -> Int
mySum [x] = x
mySum (x:xs) = x + mySum xs
~~~

## テストを書く

* `test.hs` に普通のユニットテストを書きます

    * [hspec](http://hspec.github.com/)という[RSpec](http://rspec.info/)インスパイアなテストフレームワークと
    * [HUnit](http://hackage.haskell.org/package/HUnit) という単体テストライブラリを使用

~~~ {.haskell}
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "sum function" $ do
    it "is correct" $ do
      mySum [1, 2, 3] `shouldBe` 6
      mySum [1 .. 10] `shouldBe` 55
~~~

思いつくままにテストケースを書きます

## テストを実行

~~~ {.bash}
> cabal build
Building qc-test-0.1.0.0...
Preprocessing executable 'qc-test' for qc-test-0.1.0.0...
[1 of 1] Compiling Main             ( main.hs, dist\build\qc-test\qc-test-tmp\Main.o )
Linking dist\build\qc-test\qc-test.exe ...
Preprocessing test suite 'hoge-test' for qc-test-0.1.0.0...
[1 of 1] Compiling Main             ( test.hs, dist\build\hoge-test\hoge-test-tmp\Main.o )
Linking dist\build\hoge-test\hoge-test.exe ...
> cabal test
Running 1 test suites...
Test suite hoge-test: RUNNING...
Test suite hoge-test: PASS
Test suite logged to: dist\test\qc-test-0.1.0.0-hoge-test.log
1 of 1 test suites (1 of 1 test cases) passed.
~~~

パスしたようですね！

## QuickCheck のテストを書く

* QuickCheckのテストケースは関数（プロパティという）

    * **「何か」** （ここでは `Int` のリスト） を受け取り、`Bool` を返す関数
    * QuickCheckが、関数の引数（テストデータ）を適当にたくさん生成する
    * すべてのテストデータについて `True` が帰ればパス

~~~ {.haskell}
prop_mySum :: [Int] -> Bool
prop_mySum xs =
  mySum xs == sum xs -- sum は標準のリストの総和を求める関数
~~~

簡単でしょう？

## テストフレームワークに組み込み

* prop 関数に記述とプロパティを渡す

~~~ {.haskell}
import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck -- これを追加
-- (略)

main :: IO ()
main = hspec $ do
  describe "sum function" $ do
    it "is correct" $ do
      mySum [1, 2, 3] `shouldBe` 6
      mySum [1 .. 10] `shouldBe` 55

    prop "is equivalent to sum" $ -- これを
      prop_mySum                  -- 追加
~~~

## 別な書き方

* 直に書いても良い

~~~ {.haskell}
import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck
-- (略)

main :: IO ()
main = hspec $ do
  describe "sum function" $ do
    it "is correct" $ do
      mySum [1, 2, 3] `shouldBe` 6
      mySum [1 .. 10] `shouldBe` 55

    prop "is equivalent to sum" $ \xs ->
      mySym xs == sum xs
~~~

## QuickCheck テストの実行

* テストフレームワークが面倒を見てくれるので、同じように実行するだけ！

~~~ {.bash}
> cabal build && cabal test
Running 1 test suites...
Test suite hoge-test: RUNNING...

sum function
 - is correct
 - is equivalent to sum FAILED [1]

1) sum function is equivalent to sum FAILED
*** Failed! Exception: 'test.hs:(6,1)-(7,27): Non-exhaustive patterns in function mySum' (after 1 test):
[]
...
0 of 1 test suites (0 of 1 test cases) passed.
~~~

おや(棒)、テストがコケたみたいですね

## テストの結果

* どのテストが、どの入力で、どういう理由でこけたかが出力

~~~ {.haskell}
   - is equivalent to sum FAILED [1]
~~~

~~~ {.haskell}
> 1) sum function is equivalent to sum FAILED
> *** Failed! Exception: 'test.hs:(6,1)-(7,27): Non-exhaustive patterns in function mySum' (after 1 test):
> []
~~~

どうやら、空のリストの時にエラーになっているようです

~~~ {.haskell}
mySum :: [Int] -> Int
mySum [x] = x    ←   あっ(´･_･`)
mySum (x:xs) = x + mySum xs
~~~

## 修正＆再実行

~~~ {.haskell}
mySum [] = 0
mySum (x:xs) = x + mySum xs
~~~

直して、再実行

~~~ {.bash}
> cabal test
Running 1 test suites...
Test suite hoge-test: RUNNING...
Test suite hoge-test: PASS
Test suite logged to: dist\test\qc-test-0.1.0.0-hoge-test.log
1 of 1 test suites (1 of 1 test cases) passed.
~~~

ふう、事なきを得ました。

## C++のコードをテストする

* C++のコードをQuickCheckする

* HaskellからC++のコードを呼び出せば良い

    * HaskellからC++（C）のコードを呼び出すのはとても簡単
    * FFI (Foreign Function Interface) を用いる

## HaskellからC++コードを呼び出す

* まず、C++のコードを書きます

~~~ {.cpp}
int add(int x, int y)
{
    return x * y; // わざと間違えてます！！
}
~~~

## .cabal ファイルを編集

~~~ {.haskell}
test-suite hoge-test
  type:                exitcode-stdio-1.0
  main-is:             test.hs
  build-depends:       base ==4.5.*
                     , hspec
                     , QuickCheck
  -- ↓ C++ファイルを c-sources に列挙して外部ライブラリにstdc++を追加
  c-sources:           hoge.cpp
  extra-libraries:     stdc++
~~~

## C++を呼び出すコードを書く

`foreign import ccall` で `cdecl` 呼び出し規約の関数を呼び出せる

~~~ {.haskell}
{-# LANGUAGE ForeignFunctionInterface #-}
import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property

main :: IO ()
main = hspec $ do
  describe "add function" $ do
    prop "it corrects" $ \x y -> do
      morallyDubiousIOProperty $ do -- テスト中でIOをやるための関数
        z <- add x y
        return $ z == x + y

foreign import ccall add :: Int -> Int -> IO Int
~~~

## テスト実行

~~~ {.haskell}
> cabal test
Running 1 test suites...
Test suite hoge-test: RUNNING...

add function
 - it corrects FAILED [1]

1) add function it corrects FAILED
*** Failed! Falsifiable (after 1 test and 2 shrinks):
0
1


Finished in 0.0120 seconds, used 0.0156 seconds of CPU time

1 example, 1 failure
Test suite hoge-test: FAIL
Test suite logged to: dist\test\qc-test-0.1.0.0-hoge-test.log
0 of 1 test suites (0 of 1 test cases) passed.
~~~

## C++コードをQuickCheck

~~~ {.haskell}
#include <vector>
using namespace std;

int vsum(const vector<int> &v) {
  int ret = 0;
  for (size_t i = 0; i < v.size(); ++i)
    ret += v[i];
  return ret;
}
~~~

これをテストしたいが、Haskellのデータ構造とのマーシャリングが面倒なので、
ラッパ関数を書く

~~~ {.haskell}
extern "C" int c_vsum(int *p, int n) {
  return vsum(vector<int>(p, p+n));
}
~~~

## テストコード

~~~ {.haskell}
{-# LANGUAGE ForeignFunctionInterface #-}
import Test.Hspec
import Test.Hspec.HUnit
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Foreign
import Foreign.C

main :: IO ()
main = hspec $ do
  describe "add function" $ do
    prop "it corrects" $ \xs -> do
      morallyDubiousIOProperty $ do
        withArrayLen (map fromIntegral xs) $ \len ptr -> do
          ret <- c_vsum ptr (fromIntegral len)
          return $ fromIntegral ret == (sum xs :: Int)

foreign import ccall c_vsum :: Ptr CInt -> CInt -> IO CInt
~~~

## テスト実行

~~~ {.haskell}
> cabal test
Running 1 test suites...
Test suite hoge-test: RUNNING...
Test suite hoge-test: FAIL
Test suite logged to: dist\test\qc-test-0.1.0.0-hoge-test.log
0 of 1 test suites (0 of 1 test cases) passed.
~~~

C++のテストがQuickCheckできました(´\^_\^`)!


# ベンチマーク

## [Criterion](http://bos.github.com/criterion/)

* Haskellのベンチマークフレームワーク

    * カッコいいレポートページが出る

<iframe src="http://bos.github.com/criterion/"></iframe>

## Criterionの特徴

* 時間計測のオーバーヘッドの計測と除去

* 多数回の繰り返しとブートストラップ法による統計データ計算

* 短い実行時間のプログラムを正確に計測できる

* かっこいいレポートページが出る！

C++でも使いたい！

## C++をCriterion

* こいつにC++を食わせると…？

とりあえず適当にソートプログラム書いてみた

~~~ {.cpp}
#include <vector>
#include <algorithm>
using namespace std;

extern "C"
void vsort(int *p, int n)
{
  vector v(p, p+n);
  sort(v.begin(), v.end());
}
~~~

## ベンチマークを cabalファイルに追加

~~~ {.haskell}
benchmark hoge-bench
  type:                exitcode-stdio-1.0
  main-is:             bench.hs
  build-depends:       base ==4.5.*
                     , criterion
                     , random
  c-sources:           hoge.cpp
  extra-libraries:     stdc++
~~~

## Criterion書く

~~~ {.haskell}
{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Monad
import Data.List
import Foreign
import Foreign.C
import System.Random

import Criterion.Main

main = do
  let len = 1000
  xs <- replicateM len $ randomRIO (0, 1000)
  defaultMain
    [ bgroup "sort"
      [ bench "c++"     $ whnfIO $ withArrayLen (map fromIntegral xs) $ \len ptr ->
         vsort ptr (fromIntegral len)
      , bench "haskell" $ nf sort (xs :: [Int])
      ]
    ]

foreign import ccall vsort :: Ptr CInt -> CInt -> IO ()
~~~

## ベンチマーク実行

~~~ {.haskell}
> cabal bench
Running 1 benchmarks...
Benchmark hoge-bench: RUNNING...
warming up
estimating clock resolution...
...

benchmarking sort/c++
mean: 54.11847 us, lb 53.89714 us, ub 54.40775 us, ci 0.950
std dev: 1.288492 us, lb 1.016472 us, ub 1.923825 us, ci 0.950
found 5 outliers among 100 samples (5.0%)
  4 (4.0%) high mild
  1 (1.0%) high severe
variance introduced by outliers: 17.107%
variance is moderately inflated by outliers

benchmarking sort/haskell
mean: 363.3632 us, lb 361.8362 us, ub 365.6127 us, ci 0.950
std dev: 9.413442 us, lb 7.040128 us, ub 14.36999 us, ci 0.950
found 4 outliers among 100 samples (4.0%)
  3 (3.0%) high mild
  1 (1.0%) high severe
variance introduced by outliers: 19.973%
variance is moderately inflated by outliers
Benchmark hoge-bench: FINISH
~~~

## 結果

[結果](./result.html)

<iframe src="./result.html">
</iframe>

# まとめ

## まとめ

* QuickCheckでC++でもハッピープロパティテストライフを

* ベンチも取れるよ

* Haskellのツールチェインたのしい
