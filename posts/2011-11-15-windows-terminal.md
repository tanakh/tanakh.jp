---
title: Windows上で最高のターミナルを構築する方法
description: Windowsでのまともなターミナル環境を探索する
tags: windows, devenv
---

皆さん、Windowsでコマンドライン使ってますか？
まともなターミナルエミュレータすらないと思っている方もいらっしゃるかもしれませんが、案外何とかなるものです。

<a href="/img/posts/mintty.png">![MINTTY](/img/posts/mintty.png)</a>

これが完成図です。1週間ほどいろいろ試した結果、動作、見た目、フォントレンダリング、どれもおおむね満足行く結果になりました。

## Terminal Emulator選び

Windowsをお使いの皆様ならご存じかとは思いますが、デフォルトのTerminal Emulatorである cmd.exe が良くないです。挙動、フォント云々の話をするまでもなく、横幅が80文字固定なのがどうしようもありません。

[この辺](http://en.wikipedia.org/wiki/List_of_terminal_emulators)を見てWindowsで動作するものを一通り試してみたのですが、動作がおかしいか、フォントが気に入らないか、プロプライエタリかで、どれもよくないなあという結論を一度は下しました。

それでVMWare PlayerでLinuxを入れてgnome-terminalをUnityモードで使っていたのですが、Gnome3になったりVMWarePlayerが新しくなってVMWareToolsがうまく動かなくなったりして、どうしたものかなあというときに、ふと起動してみた [mintty] が案外良かったので、そのまま移行してしまいました。

## mintty とは

[mintty] はCygwinのターミナルエミュレータです。あるバージョンからCygwinに同梱されるようになっていて、当時はなんじゃこりゃ、と思った記憶があります。それがバージョンアップを繰り返して最近1.0.0になっていたようなのですが、非常に使い勝手が良くなっていました。動作にもおかしなところはなく、それもそのはず、minttyは [putty] のターミナル部分をベースに開発されているそうなのでした。しかし、puttyはずいぶん昔に更新が止まっており（最近約4年ぶりに新しいバージョンが出ましたが）、WindowsVista/7での高品位なフォントレンダリングに対応していなかったり、いろいろ不満がありました。

また、minttyは最近では [MSYS] にも対応したようで、Cygwinをインストールせずに利用することもできます。MSYSのパッケージマネージャでインストールすることも可能で、Windowsのターミナルエミュレータとしてかなり使い勝手の良いものになっています。

## 環境最速構築手順

ここではMSYSのminttyをインストールすることにします。

### MinGW/MSYSのインストール

[MSYS] のインストールは、インストーラを使うのが便利です。以下のURLから新しいものをダウンロードしてください。

<http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/>

インストーラを実行すればMinGW/MSYSのインストールは完了です。インストールの際にMSYS Basic Systemのところにチェックを入れるのを忘れずに。

### minttyのインストール

MSYSのインストールが完了したら、MinGW Shellを起動して、以下のコマンドを実行します。

~~~ {.bash}
$ mingw-get update
$ mingw-get install mintty
~~~

これで mintty のインストールは完了です。MinGWも便利になったもんですね。mintty コマンドを起動できる用になったはずです。起動した mintty をタスクバーにピン留めしておけば一発で起動できるようになって便利です。

### $HOME の設定

sshやemacsなどが設定ファイルを参照できるように、$HOME環境変数を設定しておきます。$HOMEの値は c:\Users\<ユーザ名> にします。そこに .ssh/ ディレクトリを作成して鍵を置いておけば mintty からSSHができるようになります。

### フォントの設定

minttyはデフォルトではあまりいい感じのフォントとウインドウ表示ではないので、お好みの見た目になるように調節します。

まずはフォントですが、私のおすすめは [Inconsolata] です。Inconsolataは [Top 10 programming fonts](http://hivelogic.com/articles/top-10-programming-fonts/) でも1位として挙げられている非常に美しいフォントです。

![Inconsolata](http://www.levien.com/type/myfonts/incoshow.png)

Inconsolataは本家から取得しても良いですが、[Google Web Fonts](http://www.google.com/webfonts) にあるものはヒンティングの修正が行われており、Clear Type環境下でより高品位なレンダリングが期待できるので、こちらから取得するのがおすすめです。

### フォントリンク

Inconsolataは英文フォントなので、日本語は（WindowsXP以降では）フォントリンクされたフォント、デフォルトではMS ゴシックでレンダリングされます。これはあまりうれしくありません。というのもMS ゴシックは小さいサイズだとビットマップフォントになるのと、ヒンティングがないからです。

そこで、Inconsolataにメイリオをリンクさせるのがおすすめです。フォントリンクを設定するにはレジストリにエントリを追加する必要があります（フォントリンクに関して詳しくは[このあたりを](http://blog.livedoor.jp/mailnobu/archives/16399739.html)参照）。

`HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontLink\SystemLink` に Inconsolata という名前のキーを複数行文字列データとして作成し、meiryo.ttc,Meiryo という行を追加すればOKです。

フォントリンクするのが面倒なら、プログラミング用フォント [Ricty]を利用するのがお手軽です。

![Ricty](http://save.sys.t.u-tokyo.ac.jp/~yusa/fonts/print_ricty/print_ricty_thumbnail.png)

Rictyは、Inconsolataと [Migu] の合成フォントです。Miguも非常に視認性の高い日本語フォントですが、個人的にメイリオの字形の方が好みなのと、Inconsolataにあったヒンティングが損なわれているようで、Windowsだとフォントがあまりきれいにレンダリングされないので、私はInconsolataにメイリオをリンクする方が好きです。

### ウインドウの設定

フォントが設定できたら、後はウインドウの見た目をお好みにいじります。お好みでいいですが、LooksのTransparencyのところをGlassにするのが個人的お勧めです。AeroUIの磨りガラス風UIがウインドウ全体になって、なんだか無駄にかっこいいです。デフォルトでは背景が透けすぎるので、Windowsのウインドウの設定から、色を濃いめにした方がいいでしょう。

<a href="/img/posts/mintty2.png">
![mintty-glass](/img/posts/mintty2.png)
</a>

特に何もなくとも適当にhtopを表示しておくとなんだか楽しいです。

## その他

minttyにはウインドウ分割機能がないので、ログイン先サーバにscreen/tmuxなどをインストールしておくと便利です。

私は最近はbyobu-tmuxを利用しています。最初のスクリーンショットのようにウインドウを分割して使えます。ウインドウの移動がShift+Left/Rightでできたりして割と直感的に操作できます。

## まとめ

gnome-terminal on VMWare からの移行ですが、おおむね満足です。

良かったところ：

* フォントがきれい
* 無駄にかっこいい
* 軽い
* WindowsのIMEが使える

不満点：

* タブがない（tmuxがあればそんなに困らないけど）

というわけで、mintty、おすすめです。

[mintty]: http://code.google.com/p/mintty/
[putty]: http://www.chiark.greenend.org.uk/~sgtatham/putty/
[MSYS]: http://www.mingw.org/wiki/MSYS
[Inconsolata]: http://www.levien.com/type/myfonts/inconsolata.html
[Ricty]: http://save.sys.t.u-tokyo.ac.jp/~yusa/fonts/ricty.html
[Migu]: http://mix-mplus-ipa.sourceforge.jp/
