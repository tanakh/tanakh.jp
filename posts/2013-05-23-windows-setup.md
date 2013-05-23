---
title: 最近のWindowsの開発環境のセットアップ
description: 最近のWindowsセットアップのための自分用メモ
tags: windows, devenv
---

周囲にWindowsユーザがめっきり減ってきた昨今ですが、
Windowsユーザの皆様はいかがお過ごしでしょうか。

Windows8は使えないだの、
シェルがしょぼいからあれだのと言われることも多いですが、
圧倒的にたくさんのPCで安心して動かせるOSとして、
私個人としてはとても便利に使っています。

Let'snoteのCF-S10Dという2年ほど前の機種を使っているのですが、
ようやくPanasonicのWindows8サポートがこの機種までやってきたので、
Windows8に入れ替えることにしました。
実は発売当初にもWindows8を入れていたのですが、
Let'snoteを快適に使うには必須の、「くるくるホイール」が使えなかったり、
謎の認識されないデバイスがあったりだったので、
Windows7に戻していました。

というわけで、セットアップついでにそのときの記録を書いておこうと思います。

## Windows8について

Windows8は、なんだかんだいわれてますが、
Windows7より目に見えてパフォーマンスがよくなってるので、とても快適です。
Windows9xの頃からろくに改善されてこなかった、
エクスプローラやら、タスクマネージャやら、ファイルコピーダイアログやらが改善されて、
デフォルトでISOマウント機能があったり、PDFリーダがあったり、
Windows Defenderというマルウェア対策ソフトが入っていたり、
地味なところが良くなっています。
スタートメニューは、どうなんでしょうかね。
Windowsボタンを押せばいい気がします。

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130077&authkey=ACvjI--JZB8wRhM" width="319" height="271" frameborder="0" scrolling="no"></iframe>

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130063&authkey=AM_VvnGVhmJjxFU" width="320" height="240" frameborder="0" scrolling="no"></iframe>

入れ替え記念のスクリーンショット。
GPU以外は二年前のノートPCとは思えないスペックだなあ。

あんまり関係ないけど、[CrystalDiskMark](http://crystalmark.info/software/CrystalDiskMark/)
が時代の波にのまれて萌え化してた。

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130062&authkey=ADED2ukIQv5InWw" width="320" height="274" frameborder="0" scrolling="no"></iframe>

スタート画面は、タイルは見なかったことにして、もっぱらテキストでの検索を使えば便利です。
ショートカット系の機能が充実しているので、それらを駆使すれば、
Windows7よりも検索機能が強力な分、便利に使えると思います。
それらを使わない場合、タッチパネルがあればよいのですが、
なければマウスのストロークが大変なので、
初心者向けにはちょっと不親切になっているのかもしれません。
意外だったのは、普通のノートPCでも、タッチパネルがついていると非常に便利に使えるということです。
ボタンを押すのとかも、タッチパッドでマウスを動かして押すよりも、
見えているものを指で押した方が早いというのは、
考えてみれば当然の話ではあります。

メトロアプリは現状生煮え感がすごいので、
全く使えていません。
Windows8.1で何とかなればいいのですが。
そもそも、CF-S10Dでは画面の解像度が足りてなくて、
画面の横にアプリを固定する機能が使えないという、
悲しい現状があります。
マシンスペックはここ最近大して変わっていないのに、
画面の解像度だけはだいぶ変わりましたね。

## 下準備

開発環境を整える前に、下準備としていくつかツールを適当に放り込みます。

- Explzh <http://www.ponsoftware.com/archiver/download.htm>

    アーカイバは昔からずっとこれを使っています。
    開発もコンスタントに続いていてすばらしい。
    解凍時に必要なDLLを勝手に取ってきてくれます。
    64ビット版は対応DLLが少ないので、64ビットOSでも32ビット版で。
    なぜかこのソフトを入れると、
    エクスプローラ拡張でシンボリックリンクを貼る機能を追加してくれるので、大変便利。
    後の方で使います

- KeySwap for XP <http://www.vector.co.jp/soft/winnt/util/se228667.html>

    CapsLockをCtrl化するのに利用。
    インストール不要でさくっと入れ替え。
    名前に反してVista以降でも問題なく使える。

- Rapid Environment Editor <http://www.rapidee.com/en/about>

    環境変数をいじるソフト。
    最近Twitterで「Windowsの環境変数設定ダイアログいつまでたってもしょぼいままだなあ」
    とぼやいていたら教えていただいたソフト。
    あまりにも便利で、今までの苦労は何だったのかという。

    <iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130078&authkey=ALD_sH29Qslt1_Y" width="320" height="221" frameborder="0" scrolling="no"></iframe>

    パスを配列として編集できたり、パスをダイアログから選択できたり、
    存在しないパスが含まれる変数に対してwarningが出たりするので、
    アンインストールしても環境変数を直さないお行儀の悪いソフトのお掃除にも便利です。

- Clover <http://ejie.me/>

    エクスプローラをChromeそっくりにタブ化するソフト。
    キーバインドもChromeと全く同じなので、非常に使い易いです。
    ブックマーク機能もあって、ここによくアクセスするフォルダを突っ込んでおくと、
    非常に捗ります。

    <iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130069&authkey=AB-3OV8x4qAT6lA" width="320" height="224" frameborder="0" scrolling="no"></iframe>

## Chocolatey <http://chocolatey.org/>

ようやく、開発環境編です。

まずはこれ。[Chocolatey](http://chocolatey.org/)です。
[NuGet](http://nuget.org/)のコマンドラインフロントエンドらしく、
要するに、Wndows版apt-getのようなものです。

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130065&authkey=ALHouBA3wdeC7xI" width="319" height="215" frameborder="0" scrolling="no"></iframe>

「Windowsのソフトは、インストーラ探しに行くのが面倒」
とお嘆きの皆さんも、これでだいぶ救われると思います。
前のところで紹介したソフトは登録されていなかったり、まだまだパッケージが少ないようですが、
開発環境周りはやたら充実しています。
[VisualStudio](http://chocolatey.org/packages/VisualStudio2012WDX)やら、
[IE](http://chocolatey.org/packages/ie9)の各バージョンやらも、
コマンドラインでインストールできるみたいで、
ちょっとびっくりしました。

Chocolatey自体のインストールはとても簡単で、トップページにばばーんと書いてあるコマンドを
`cmd.exe` に貼り付けるだけです。

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130066&authkey=AJtT3jIa3MK-cuY" width="320" height="233" frameborder="0" scrolling="no"></iframe>

あとは、powershellなりcmdなりから、

~~~
> chocolatey install <ほげほげ>
~~~

もしくは

~~~
> cinst <ほげほげ>
~~~

と打ち込めば、いろいろインストールできます。

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130068&authkey=AGeE-VGSneKoAMU" width="320" height="233" frameborder="0" scrolling="no"></iframe>

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130067&authkey=AP8ZTE97ZNyTeDA" width="320" height="233" frameborder="0" scrolling="no"></iframe>

今回インストールしたものを列挙しておきます。

- [GoogleChrome](http://chocolatey.org/packages/GoogleChrome), [Firefox](http://chocolatey.org/packages/Firefox)

    とりあえずブラウザ一式は必要だろうか。

- [dropbox](http://chocolatey.org/packages/dropbox)

    何はなくともDropbox。
    後でエディタの設定共有に使います。

- [mingw](http://chocolatey.org/packages/mingw)

    Windowsでもあると便利なGNU環境。
    いろいろありますが、とりあえずMinGW。
    ちなみに、ChocolateyではCygwinの特別サポートがあって、
    `chocolatey cygwin` または `ccygwin` コマンドで、
    Cygwin上のパッケージ管理ができたりします。

    mingwでもその辺は別に困らなくて、
    mingwインストール後に `mingw-get install` コマンドでいろんなパッケージを入れられます。

    Cygwinを使う場合は、本家よりも
    [gnupack](http://gnupack.sourceforge.jp/docs/latest/UsersGuide.html)
    を使うのがとても便利です。
    zipを落として展開するだけで、それなりにナイスに設定されたCygwinをインスタントに使えます。
    それなりにナイスに設定されたemacsとvimも付いてきます。

- [git](http://chocolatey.org/packages/git)

    何はなくともgit。
    git関係のパッケージいろいろありますが、素直にこれでいいと思います。
    [Github for Windows](http://windows.github.com/)とかもありますが、
    GUIが特にいらない人はこれで良いかと。

- [sublimetext2](http://chocolatey.org/packages/sublimetext2)

    今時のエディタ。今のメインエディタ。後の方で設定を記しておきます。
    emacsやvimがお好きな方は、これもChocolateyでインストールできます。
    ダウンロード数から見ると、vimの方が人気が高いようですね。

- [ConEmu](http://chocolatey.org/packages/ConEmu)

    Windowsでコマンドラインを使うに当たって、
    入れておいて損はない、高機能ターミナルエミュレータ。

    [一年半前の記事](/posts/2011-11-15-windows-terminal.html)では、
    [mintty](https://code.google.com/p/mintty/)を推していましたが、
    今は[ConEmu](https://code.google.com/p/conemu-maximus5/)を使っています。

    どちらも甲乙つけがたいのですが、今はConEmuかなあという感じです。
    Chocolateyでは、Console2というのが一番人気のようですが、
    日本語環境では全然だめっぽいのと、UIがいまいちいけてないです。
    その点、minttyとConEmuはどちらもクール。

Chocolateyは、Cygwinだけでなく、そのほかいくつかのツールに関して、
特別なパッケージサポートが組み込まれています。
よく使いそうなのは、RubyとPythonでしょうか。
それぞれ `cgem` と `cpyton` で利用できます。
Node.jsなどもさくっと入るので、LLユーザの方には大変便利だと思います。

## ConEmuの設定

ConEmuはデフォルトでは`cmd.exe`をシェルとして使う設定になっていますが、
`cmd.exe`、PowerShell、MinGWのbash、
この三色のシェルを一発起動できるようにしておくと、大変クールです。

設定の、Startup → Taksに三つを登録しておきます。

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130073&authkey=ALmw7uGtBBG3V9o" width="319" height="236" frameborder="0" scrolling="no"></iframe>

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130071&authkey=AINSh3AKd3Db7Ko" width="319" height="236" frameborder="0" scrolling="no"></iframe>

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130079&authkey=AL6mL78YDUheVfs" width="319" height="236" frameborder="0" scrolling="no"></iframe>

bashは、bash.exeに `--login -i` パラメータを渡してやります。

~~~
C:\MinGW\msys\1.0\bin\bash --login -i
~~~

powershellとcmdはそのまま適当にコマンドを貼ればOK。
デフォルトのシェルをbashに変更しておくと、さらに便利です。

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130072&authkey=AGyCQ5CZq_xj83w" width="319" height="236" frameborder="0" scrolling="no"></iframe>

ちなみに、bashはHOME環境変数を参照するので、
これも設定しておきましょう。
HOMEは、Windowsの個人アカウントのホームと共有するのがおそらく便利です。
Rapid Environment Editorで、`HOME=C:\Users\<ユーザ名>`を追加します。

ついでに、`LANG=C`を入れておくと、GNU系ツールが死ぬ確率がちょっと下がります。

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130070&authkey=APrs8Xu4kujkgQY" width="319" height="244" frameborder="0" scrolling="no"></iframe>

こんな風に特に問題なくbashできています。

## エディタの設定

SublimeText2を複数の環境で利用しているのですが、
毎度設定していてはとても面倒なので、
Dropboxを使って設定を同期するようにしています。

Dropboxの方に、設定ディレクトリの本体をおいて、
ローカルマシンにはシンボリックリンクをおいておきます。
こうすれば、勝手にインストールしたパッケージやら設定やらが同期されます。

Windowsでのシンボリックリンク作成は、
普通にやるとちょっと面倒なのですが、最初に紹介したExplzhをインストールすると、
なぜか右クリックメニュー拡張にシンボリックリンク作成が追加されるので、
これを使うと楽ちんです。

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130075&authkey=ALW2hR-sAopOYXY" width="319" height="198" frameborder="0" scrolling="no"></iframe>

<iframe src="https://skydrive.live.com/embed?cid=4E77147A7E5C73B7&resid=4E77147A7E5C73B7%21130076&authkey=AIjxBw7xtnPJPqo" width="320" height="213" frameborder="0" scrolling="no"></iframe>

ちなみに、Windowsアプリの、アプリ固有設定置き場のデフォルトは、`%APPDATA%`です。
エクスプローラのロケーションバーに`%APPDATA%`と打ち込むと、
展開されたフォルダ名が開きます。

私はemacsも併用していて、こちらの設定もDropboxで同期しています。
`.emacs`を`.emacs.d/init.el`に変更して、
`.emacs.d`に設定を一本化すれば楽です。
emacs24以降だとパッケージマネージャがデフォルトで含まれており、
インストールしたパッケージはこのディレクトリ以下に入るようになるので、
パッケージも共有できます。

あとは、Sublimeの時と同様にシンボリックリンクを貼るだけです。
emacsはGNU流のソフトなので、
`%APPDATA%`ではなく`%HOME%`に貼る必要があることに注意します。

## 以降の設定

各自利用するツールなり、プログラミング言語なりをインストールします。
私の場合ですと、Haskell Platformとかを適当に入れました。
エディタの言語向け設定とか、いろいろありますが、
それはまたの機会に。
