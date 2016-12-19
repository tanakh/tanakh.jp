---
title: Run Rust code on PEZY-SC processor
description: RustのコードをPEZY-SCというプロセッサーで動かしてみる
tags: rust
---

これは[Rust その2 Advent Calendar](http://qiita.com/advent-calendar/2016/rust-lang-2)の16日目の記事です。日付と投稿日がかみ合っていなくてすみません。

# 概要

[PEZY-SC](http://pezy.co.jp/products/pezy-sc.html)というメニーコアプロセッサーでRustのコードを動かしてみたというお話です。

# PEZY-SCとは

PEZY-SCとは、[PEZY Computing](http://pezy.co.jp/)が開発したメニーコアプロセッサーです。1024コアのRISCプロセッサーで、各コア8スレッドのSMTになっており、トータルで8192スレッドが同時に動きます。ピーク性能は倍制度1.5TFlops、単精度3TFlopsで、これを用いたシステムが電力効率の良いスパコンとしてGreen500などで良い成績を収めています。現在さらに性能を向上させたPEZY-SC2を開発中です。

高い演算スループットと電力効率を目指しながらも、SIMDを用いない完全なMIMDプロセッサーで、ある意味コンピューター科学の常識に反しているようなユニークな設計だと思います。

現在のところは残念ながら広く一般に利用できる状況にはありません。というかこんな記事を書いておきながら言うのもなんですが、この記事を見てくださっているほとんどの人は使ってみるすべがないと思います。Shoubuというスパコンの一般利用が始まっているので、[こちらの利用申請](http://accc.riken.jp/shoubu_info/application/) から申し込んでいただくと、もしかしたら使えるかもしれません。興味のある方はぜひ。

# 現状のPEZY-SCのプログラム開発環境

OpenCLのAPIのようなAPIが用意されており、それでホストとデバイス向けのコードを書いて実行します。

ホストコードは一般的なOpenCLのプログラムと同様に、デバイスオブジェクトやプログラムオブジェクトの作成を行って、デバイスのカーネル関数を起動します。

デバイスコードはOpenCLで用いられるOpenCL Cではなく、普通のC++で記述します。言語としては普通のC++が使えますが、ランタイムのサポートがないので、標準ライブラリの大部分は使えません。とはいえ、テンプレート関数やラムダ式などは問題無く使えますし、関数の再帰呼び出しやif文などもGPGPUとは違い気兼ねなく使うことができるので、かなり普通のプログラミングと同じような感覚でプログラムを書くことができます。

# PEZY-SCでRustを動かす方針

さて、いよいよ本題のRustですが、実はもうすでに動かすための要素は世の中に存在しているので、それらを適当に繋げるだけで動いてくれるはずです。

まず、PEZY-SCのSDKでは、コード生成をLLVMのバックエンドとして実装しています。フロントエンドとしてClangを使っているので、それでC++のコードからPEZY-SC向けにコンパイルできるという形です。

ということは、RustからLLVMのIRなりBitcodeなりを出力できれば、それをバックエンドに食わせて、PEZY-SCのネイティブコードが出力できるはずです。そして都合が良いことに、RustはバックエンドとしてLLVMを用いています。コンパイラにLLVMのIRやBitcodeを出力するオプションもあります。つまりそれらを使うだけで、Rust to PEZY-SCのネイティブコンパイラが完成します。世の中便利になったものです。

また、通常はコンパイルしたコードを動かそうと思うと、各言語処理系のランタイムを移植する必要が出てきますが、これも非常にありがたいことに、Rustはとてもランタイムが小さい言語になっています。PEZY-SCで動かすにあたって特にうれしい点として、GCが無いというのがあります。GCがないので、当たり前ですがGCのコードを移植する手間が無くなり、また、処理系のためのメモリ管理のコードも必須ではなくなります。

それに加えて、RustはCの代替を本気で狙いに行っている言語なので（？）、標準ライブラリを無効にして、ランタイムへの依存を極限にまで小さくするオプションが用意されています。これを利用すると、Rustを動かすために必要なランタイムはごく少数の関数のみになります。

とは言え、こうすれば理屈の上では動くはずだ、というのと、実際に動く、との間にはものすごく大きな違いがあって、動くはずのものがなぜかすんなりとは動かないというのが世の常ですから、実際にお手軽にできるのかやってみる必要があります。実際にお手軽にできたら、Rustの理想は本当なんだと改めて実感することができるでしょう。

# RustのコードをLLVM IRにコンパイルする

まずは小さいコードで、コンパイルできる環境を作っていきます。

C++で適当なPEZY-SCのコードを用意します。

```c++
void pzc_add(int32_t a, int32_t b, int32_t *c) {
    *c = a + b;
    flush();
}
```

与えられた引数を足し合わせるだけの、取るに足らないコードです。これをRustで書いてみます。

`flush()`というのは、メモリキャッシュのフラッシュを行います。PEZY-SCではプログラム終了時にキャッシュの中身が書き出されないので、明示的に呼んでやる必要があります。またPEZY-SCのSDKでは、ホストコードから起動される関数の名前の先頭は`pzc_`で始まるというのを期待しているので、それに合わせてやります。

```rust
pub fn pzc_add(a: i32, b: i32, c: *mut i32) {
    unsafe {
        *c = a + b;
    }
    flush();
}
```

ほとんど直接的に翻訳できるのですが、Rustではポインタを扱う操作は自動的にunsafeになるので、そこを`unsafe`で囲んでやる必要があります。

現在のPEZY-SCのSDKの都合なのですが、OpenCLのAPIからデバイスコードの関数の名前をルックアップする際に「C++でmangleされた後の名前を期待する」という（個人的には外したい）仕様があるので、それに合わせると、

```rust
#[no_mangle]
pub fn _Z7pzc_add(a: i32, b: i32, c: *mut i32) {
    unsafe {
        *c = a + b;
    }
    flush();
}
```

このようになります。`#[no_mangle]`は、関数名のmanglingを行わないように指示するRustのプラグマです。このままでは`flush()`が無いといってコンパイルがこけるので、とりあえず適当に定義しておきます。また、余計な依存を発生させないように`#![no_std]`をつけておきます。

```rust
#![no_std]

fn flush() {}

#[no_mangle]
pub fn _Z7pzc_add(a: i32, b: i32, c: *mut i32) {
    unsafe {
        *c = a + b;
    }
    flush();
}
```

これを`rustc`でコンパイルします。


```sh
$ rustc -O --emit=llvm-ir --crate-type=lib test.rs
```

`--crate-type=lib`をつけるとライブラリとしてコンパイルされるようになります。これがなければ`main`関数がないといってコンパイルがこけます。Rustには`#![no_main]`というプラグマもありますが、その場合は使われていない関数（`pub`が付いているものも含む）の削除などの最適化が行われるので、やはりライブラリモードとしてコンパイルする必要があります。

`--emit=llvm-ir`オプションで、LLVM IRを出力させます。また、`-O`で最適化オプションを有効にしています。

コンパイルすると、次のような結果が得られます。

```asm
; ModuleID = 'kernel.cgu-0.rs'
source_filename = "kernel.cgu-0.rs"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: norecurse nounwind uwtable
define void @_Z7pzc_add(i32, i32, i32* nocapture) unnamed_addr #0 {
entry-block:
  %3 = add i32 %1, %0
  store i32 %3, i32* %2, align 4
  ret void
}

attributes #0 = { norecurse nounwind uwtable }
```

ターゲットトリプルが"x86_64-unknown-linux-gnu"になっていたり、データレイアウトがそれに合わせたものになっていたりしますが、rustcのバックエンドには当然PEZY-SCのサポートなんてものは存在しないので、見なかったことにします。PEZY-SCで実行するにあたっては、とりあえずはポインタサイズが64ビットでリトルエンディアンになっていれば問題がないはずです。

# LLVM IRをPEZY-SCのネイティブコードにする

次に、LLVM IRをLLVMのBitcodeにします。`rustc`で直接Bitcodeを出させたほうが速いような気もしますが、今回はRustcとPEZYのSDKが利用するLLVMのバージョンがかみ合わず、うまく通らなかったので、テキストで表現されるのIR経由でBitcodeを作ることにしています。

```sh
$ <path-to-pzsdk>/bin/llvm-as test.ll -o test.bc
```

ここではPEZY-SC付属の`llvm-link`を使います。これで生成されるBitcodeがSDKの想定するバージョンで生成されたものになります。

つぎに、PEZY-SCのランタイムとリンクします。デバイス関数の起動時に実行されるスタートアップルーチンなどが含まれています。

```sh
$ <path-to-pzsdk>/bin/llvm-link \
    <path-to-pzsdk>/bin/pzcrt64.pzo test.bc -o kernel.bc
```

つぎにBitcodeをPEZY-SCのアセンブリにコンパイルします。

```sh
$ <path-to-pzsdk>/bin/llc -O3 -march=pz64 -mcpu=sc -o kernel.s kernel.bc
```

これでアセンブリまでできました。

```asm
・・・(略)・・・

_Z7pzc_add:
.function _Z7pzc_add,Void,Int32,Int32,Pointer // @_Z7pzc_add
// BB#0:                                // %entry-block
        i.add   r8 r9 r8
        i64.mov         p0 x5
        i.esw   p0 0 r8
        CHECK_RET( lr )
        c.ret
.func_end16:
        .size   _Z7pzc_add, .func_end16-_Z7pzc_add

```

ここまでくれば、あとはSDKの所定の方法でアセンブルしてリンクすればデバイスコードのバイナリができます。

```sh
$ <path-to-pzsdk>/bin/clang -E -DPZ_LLVMMC_ASM -D__pezy_sc__ -xc -I <path-to-pzsdk>/inc -o kernel-expanded.s kernel.s
$ <path-to-pzsdk>/bin/llvm-mc -filetype=obj -arch=pz64 -mcpu=sc -o kernel.hex kernel-expanded.s
$ <path-to-pzsdk>/bin/pzlink -o kernel.pz kernel.hex

```

# Intrinsic関数を作る

一応バイナリを生成できましたが、`flush()`関数が空なので、このままでは正しく動きません。とはいってもそんなに難しいわけではなく、インラインアセンブリで必要な命令を呼び出してやるだけです。

```rust
#![feature(asm)]

fn flush() {
    unsafe {
        asm!("c.wflush 5" :::: "volatile");
    }
}
```

Rustでは`asm!`マクロによってインラインアセンブリを書くことができます。ただ、この機能は現在のところunstableなので、nightlyバージョンのrustcでしか使えません。nightlyバージョンを導入する必要があって少しめんどくさそうですが、[rustup](https://rustup.rs/)を使えば割と簡単に導入できます。

```sh
$ rustup install nightly
$ rustup default nightly
```

これでデフォルトがnightlyに切り替わります。

基本的にはRustの`asm!`はgccのインラインアセンブリの使用に準拠しますが、`asm volatile`に相当するものを書いたりするために、制約の4つ目の部分が追加されています。今回はここに`"volatile"`と書くことで、このインラインアセンブリを`asm volatile`扱いにしています。これはキャッシュのフラッシュの指示をメモリの操作と入れ替えられないようにするためです。

改めて先ほどのコードをコンパイルしなおすと、


```ll
; ModuleID = 'kernel.cgu-0.rs'
source_filename = "kernel.cgu-0.rs"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define void @_Z7pzc_add(i32, i32, i32* nocapture) unnamed_addr #0 {
entry-block:
  %3 = add i32 %1, %0
  store i32 %3, i32* %2, align 4
  tail call void asm sideeffect "c.wflush 5", "~{dirflag},~{fpsr},~{flags}"() #1, !srcloc !0
  ret void
}

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind }

!0 = !{i32 2}
```

命令が埋め込まれているのがわかります。

では、適当にホストから呼び出すコードを書いて実行します。

```c++
#define __CL_ENABLE_EXCEPTIONS
#include <cl/cl.hpp>

#include <iostream>
#include <fstream>

int main()
{
    string bin;
    {
        ifstream ifs("kernel.pz");
        bin = string(istreambuf_iterator<char>(ifs), istreambuf_iterator<char>());
    }

    cl::Program program(cl::Context::getDefault(), {cl::Device::getDefault()},
        {{bin.c_str(), bin.size()}});

    auto add = cl::make_kernel<int32_t, int32_t, cl::Buffer>(program, "add");
    cl::Buffer buf(CL_MEM_READ_WRITE, sizeof(int32_t));

    add(cl::EnqueueArgs(8192), 123, 456, buf).wait();

    int32_t ret;
    enqueueReadBuffer(buf, true, 0, sizeof(int32_t), &ret);
    cout << "123 + 456 = " << ret << endl;

    return 0;
}
```

（Rustでやるという記事なのに、ホストがC++で申し訳ない(´･_･`)……。まあ今回ここは本題ではないので）


```sh
$ clang++ -O2 -std=c++11 main.cpp \
    -I <path-to-pzsdk>/inc <path-to-pzsdk>/lib/libpzcl.a -lpthread \
    -o host
$ ./host
123 + 456 = 579
```

というわけで無事実行できました！

# 並列化する

ミニマムなコードを動かすことに成功したので、もうちょっと複雑なコードを動かしていきます。そもそも先ほどのコードはメニーコアプロセッサーなのに全く並列に動かす意味のないコードでした。実際のところ、一応8192スレッドが起動して動いていたのですが、すべてのスレッドが全く同じポインタに、よってたかって同じ値を書き込むという、よくわからないことをするプログラムになっていました（複数のスレッドが同時に同じメモリに書き込む場合でも、それが同じ値なら安全であるし結果は決定的であるという保障はあります）。

並列計算のHello, Worldに相当するものとして、ベクトルの加算を書いてみます。

```rust
#![feature(asm, step_by)]
#![no_std]

#[no_mangle]
pub fn _Z8pzc_vadd(a: *const f64, b: *const f64, c: *mut f64, n: usize) {
    let tid = (get_pid() * get_maxtid() + get_tid()) as isize;
    let threads = (get_maxpid() * get_maxtid()) as isize;

    for i in (tid..n as isize).step_by(threads) {
        unsafe {
            *c.offset(i) += *a.offset(i) * *b.offset(i);
        }
    }
    flush();
}
```

長さ`n`の配列`a`, `b`, `c`を受け取り、配列の各要素について`c[i] += a[i] * b[i]`を計算するという簡単なプログラムです。配列の各要素について計算に独立性があるので、並列に計算してやることができます。ここでは `tid`番目スレッドが、`tid`, `tid + スレッド数`, `tid + スレッド数*2`, ... の要素を計算することにしました。どのスレッドがどこの値を計算すればもっとも効率よく実行されるのかというのは、こういうとても単純な計算でも、プロセッサーのアーキテクチャやメモリ階層などによって変わってくるので、意外と悩ましい部分です。

ここで各スレッドは、自分が何番目のスレッドなのか、全体ではいくつのスレッドが走っているのかを知る必要が出てきます。`get_pid()`や`get_maxpid()`がそのための関数です。`pid`はプロセスIDで、自分を実行しているプロセスの番号が返ります。PEZY-SCでは最大1024個のコアが使えるので、0から1023までの値を取ります。`tid`はスレッドIDで、各コアの中で何番目のスレッドなのかを返します。各コア8スレッドのSMTなので0から7の値を取ります。それぞれ取得するニーモニックがあるので、`flush()`と同様にこれもインラインアセンブリを書いてやればOKです。

```rust
fn get_pid() -> u32 {
    let ret: u32;
    unsafe {
        asm!("i.getpid $0": "=r"(ret));
    }
    ret
}
```

今回は返り値があるので、ローカル変数を定義して、それをインラインアセンブリに渡して、結果として関数から返します。他の関数も同様です。

```rust
fn get_maxpid() -> u32 {
    let ret: u32;
    unsafe {
        asm!("i.getmaxpid $0": "=r"(ret));
    }
    ret
}

fn get_tid() -> u32 {
    let ret: u32;
    unsafe {
        asm!("i.gettid $0": "=r"(ret));
    }
    ret
}

fn get_maxtid() -> u32 {
    let ret: u32;
    unsafe {
        asm!("i.getmaxtid $0": "=r"(ret));
    }
    ret
}
```

これでコンパイルが通ります。

```asm
; Function Attrs: uwtable
define void @_Z8pzc_vadd(double* nocapture readonly, double* nocapture readonly, double* nocapture, i64) unnamed_addr #1 personality i32 (...)* @rust_eh_personality {
entry-block:
  %4 = tail call i32 asm "i.getpid $0", "=r,~{dirflag},~{fpsr},~{flags}"() #3, !srcloc !1
  %5 = tail call i32 asm "i.getmaxtid $0", "=r,~{dirflag},~{fpsr},~{flags}"() #3, !srcloc !2
  %6 = mul i32 %5, %4
  %7 = tail call i32 asm "i.gettid $0", "=r,~{dirflag},~{fpsr},~{flags}"() #3, !srcloc !3
  %8 = add i32 %6, %7
  %9 = zext i32 %8 to i64
  %10 = tail call i32 asm "i.getmaxpid $0", "=r,~{dirflag},~{fpsr},~{flags}"() #3, !srcloc !4
  %11 = tail call i32 asm "i.getmaxtid $0", "=r,~{dirflag},~{fpsr},~{flags}"() #3, !srcloc !2
  %12 = mul i32 %11, %10
  %13 = zext i32 %12 to i64
  %14 = icmp slt i64 %9, %3
  br i1 %14, label %bb16.i.preheader, label %bb10

bb16.i.preheader:                                 ; preds = %entry-block
  br label %bb16.i

bb16.i:                                           ; preds = %bb16.i.preheader, %bb16.i
  %iter.sroa.4.023 = phi i64 [ %., %bb16.i ], [ %9, %bb16.i.preheader ]
  %15 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %iter.sroa.4.023, i64 %13) #3
  %16 = extractvalue { i64, i1 } %15, 1
  %17 = extractvalue { i64, i1 } %15, 0
  %. = select i1 %16, i64 %3, i64 %17
  %18 = getelementptr inbounds double, double* %0, i64 %iter.sroa.4.023
  %19 = load double, double* %18, align 8
  %20 = getelementptr inbounds double, double* %1, i64 %iter.sroa.4.023
  %21 = load double, double* %20, align 8
  %22 = fmul double %19, %21
  %23 = getelementptr inbounds double, double* %2, i64 %iter.sroa.4.023
  %24 = load double, double* %23, align 8
  %25 = fadd double %24, %22
  store double %25, double* %23, align 8
  %26 = icmp slt i64 %., %3
  br i1 %26, label %bb16.i, label %bb10.loopexit

bb10.loopexit:                                    ; preds = %bb16.i
  br label %bb10

bb10:                                             ; preds = %bb10.loopexit, %entry-block
  tail call void asm sideeffect "c.wflush 5", "~{dirflag},~{fpsr},~{flags}"() #3, !srcloc !0
  ret void
}
```

それっぽいものが出てきました。関数宣言の所に、

```rust
define void @_Z8pzc_vadd(double* nocapture readonly, double* nocapture readonly, double* nocapture, i64) unnamed_addr #1 personality i32 (...)* @rust_eh_personality {
```

`@rust_eh_personality` というのがありますが、これはどうやらエラーハンドラーに使われるもののようです。これに関連する関数が、Rustのコアが最低限必要とするランタイムで、このあたりの関数を定義してやる必要があるのですが、どうせエラーをもらっても何も出来ないので、とりあえずはコンパイルが通るように、`undefined reference`と言われた関数だけ定義してコンパイルを通すようにしました。

```rust
#[no_mangle]
pub fn _ZN4core9panicking5panic17h194ce5d68a8f28a1E() {}
```

また、出力されたPEZY-SCのアセンブリを見ると、


```asm
	.globl	_Z8pzc_vadd
	.p2align	3
	.type	_Z8pzc_vadd,@function
_Z8pzc_vadd:
.function _Z8pzc_vadd,Void,Pointer,Pointer,Pointer,Int64 // @_Z8pzc_vadd
// BB#0:                                // %entry-block
	...
	i.select r18 r14 r18
	i.select r19 r15 r19
	i64.sflts	x9 x7
	d.mov 	a0 d4
	d.mad 	d1 d1 d2 a0
	f.esw	p0 0 f2
	f.esw	p0 4 f3
	i64.mov 	p0 x9
	c.bf LBB17_2
	...
```

`c[i] += a[i] * b[i]` の計算の部分が、 `d.mad` を使うコードにコンパイルされています。`d.mad` は Fused multiply–add と呼ばれる命令で、3つの引数`a, b, c`を取り、`a + b * c`を計算する命令です。

では、再び適当にホストプログラムを書いて実行します。

```c++
    ...
    auto vadd = cl::make_kernel<cl::Buffer, cl::Buffer, cl::Buffer, size_t>
        (program, "vadd");
    size_t n = 1024 * 1024;
    vector<double> a(n), b(n), c(n);

    mt19937 mt;
    uniform_real_distribution<double> dist(-1, 1);
    for (size_t i = 0; i < n; i++) {
        a[i] = dist(mt);
        b[i] = dist(mt);
        c[i] = dist(mt);
    }

    cl::Buffer abuf(a.begin(), a.end(), false);
    cl::Buffer bbuf(b.begin(), b.end(), false);
    cl::Buffer cbuf(c.begin(), c.end(), false);

    vadd(cl::EnqueueArgs(8192), abuf, bbuf, cbuf, n).wait();

    vector<double> cret(n);
    enqueueReadBuffer(cbuf, true, 0, sizeof(double) * n, &cret[0]);

    double error = 0;
    for (size_t i = 0; i < n; i++) {
        auto e = abs(cret[i] - (c[i] + a[i] * b[i]));
        error += e;
    }
    cout << "err: " << error << endl;
    ...
```

```sh
$ ./host
err: 0
```

1M要素のデータに対して正しく並列に計算されたようです。

# 大きめのコード：行列の乗算

では最後に、もう少しちゃんと役に立ちそうなコードとして、行列の乗算を書いてみます。

行列の乗算は非常に簡単な計算ですが、これが最も一般受けする（？）スパコンの性能ランキング[Top500](https://www.top500.org/)のベンチマークでかなり時間的に支配的な処理になっていたりするので、実際想像以上に重要だったりします。[BLAS](http://www.netlib.org/blas/)という歴史ある線形代数ライブラリでの関数名から`dgemm`と呼ばれることもあります。

`dgemm`は、ナイーブに書くと、

```rust
// Calculate alpha * A * B + beta * C
fn dgemm(alpha: f64, a: &Vec<Vec<f64>>, b: &Vec<Vec<f64>>,
    beta: f64, c: &mut Vec<Vec<f64>>
) {
    for i in (0..a.len()) {
        for j in (0..b[0].len()) {
            let mut t = 0.0;
            for k in (0..b.len()) {
                t += a[i][k] * b[k][j];
            }
            c[i][j] = alpha * t + beta * c[i][j];
        }
    }
}
```

このように非常に単純に書けるものですが、並列にきちんと性能が出るように書こうとすると、途端にものすごくややこしくなってしまいます。書いたコードを示しておきますが、内容についての解説はこの記事の趣旨の範囲を超えるので（またPEZY-SCのアーキテクチャに大きく依存するため）割愛します。

```rust
#![feature(asm, step_by)]
#![no_std]

// スレッドの同期をとる命令
fn syncL1() {
    unsafe {
        asm!("c.sync 2" :::: "volatile");
    }
}

fn syncL2() {
    unsafe {
        asm!("c.sync 3" :::: "volatile");
    }
}

// スレッドを切り替える命令
fn chgthread() {
    unsafe {
        asm!("c.chgthread" :::: "volatile");
    }
}

// コンパイラが生成する0.0のリテラルをロードするコードが遅い対策
fn get_zero() -> f64 {
    let d: f64;
    unsafe {
        asm!("d.itof $0 zr" : "=w"(d));
    }
    d
}

macro_rules! set_ax {
    ($reg: expr, $val: expr) => {
        asm!(concat!("d.mov ", $reg, " $0"): : "w"($val));
    }
}

macro_rules! add_ax {
    ($reg: expr, $val: expr) => {{
        let d: f64;
        asm!(concat!("d.add $0 ", $reg, " $1") : "=w"(d) : "w"($val));
        d
    }}
}

macro_rules! do_mad_ax_ldd_px {
    ($AX: expr, $DA: ident, $DB: ident, $LD: ident, $PTR: ident, $OFF: expr) => (
        asm!("d.mad a$4 $2 $3 a$4; d.eldd $0 $1 $5; ## DMAD A$4 ELDD P"
            : "=w"($LD): "p"($PTR), "w"($DA), "w"($DB), "i"($AX), "i"($OFF): "memory");
    )
}

macro_rules! do_mad_ax_ldd_px_ct {
    ($AX: expr, $DA: ident, $DB: ident, $LD: ident, $PTR: ident, $OFF: expr) => (
        asm!("d.mad a$4 $2 $3 a$4; d.eldd.ct $0 $1 $5; ## DMAD A$4 ELDD P CT"
            : "=w"($LD): "p"($PTR), "w"($DA), "w"($DB), "i"($AX), "i"($OFF): "memory");
    )
}

macro_rules! do_mad_ax_ldd_px_local {
    ($AX: expr, $DA: ident, $DB: ident, $LD: ident, $PTR: ident, $OFF: expr) => (
        asm!("d.mad a$4 $2 $3 a$4; d.ldd $0 $1 $5; ## DMAD A$4 LDD P LOCAL"
            : "=w"($LD): "p"($PTR), "w"($DA), "w"($DB), "i"($AX), "i"($OFF): "memory");
    )
}

macro_rules! do_mad_ax {
    ($AX: expr, $DA: ident, $DB: ident) => (
        asm!("d.mad a$0 $1 $2 a$0; ## DMAD A$0 " :: "i"($AX), "w"($DA), "w"($DB): "memory");
    )
}

#[no_mangle]
pub unsafe fn _Z9pzc_dgemm(
    M: u32, N: u32, K: u32, alpha: f64, A: *const f64, LDA: u32, B: *const f64, LDB: u32, beta: f64,
    C: *mut f64, LDC: u32
) {
    let shift = 1;

    let xOffset = get_tid() & ((1 << (3 - shift)) - 1);
    let xStep = get_maxtid() >> shift;

    let wOffset = (get_tid() >> (3 - shift)) & ((1 << shift) - 1);
    let wStep = 1 << shift;

    let mut yOffset = 0;
    let mut yStep = 1;
    let mut zOffset = get_pid();
    let mut zStep = get_maxpid();

    if zStep == 1024 {
        let sft = if K == 2048 { 4 } else { 3 };

        yStep = 1 << sft;
        yOffset = (zOffset >> (10 - sft)) & ((1 << sft) - 1);

        zOffset = zOffset & ((1 << (10 - sft)) - 1);
        zStep = zStep >> sft;
    }

    let _pTmp0 = (0x2000 + (0x2000 / wStep) * wOffset) as *mut f64;

    const MAX_C: u32 = 64;
    let mut tmpC = [0f64; MAX_C as usize];

    let blkN = (MAX_C * xStep) >> (if K <= 512 { 2 } else { 1 });
    let bK = core::cmp::min(512 / wStep, K);

    for iM in ((zOffset * wStep + wOffset) * 2..M).step_by(zStep * wStep * 2) {
        syncL2();

        for offsetN in (yOffset * blkN..N).step_by(yStep * blkN) {
            for r in tmpC.iter_mut() {
                *r = get_zero();
            }

            for iK in (0..K).step_by(bK) {
                syncL2();

                // Matrix A to local memory
                for x in (xOffset..bK / 2).step_by(xStep) {
                    let k = 2 * x + iK;

                    let idx00 = iM + k * LDA;
                    let idx01 = idx00 + LDA;
                    let idx10 = idx00 + 1;
                    let idx11 = idx01 + 1;

                    let a00 = *A.offset(idx00 as isize);
                    let a01 = *A.offset(idx01 as isize);
                    let a10 = *A.offset(idx10 as isize);
                    let a11 = *A.offset(idx11 as isize);

                    chgthread();

                    *_pTmp0.offset((4 * x + 0) as isize) = a00;
                    *_pTmp0.offset((4 * x + 1) as isize) = a10;
                    *_pTmp0.offset((4 * x + 2) as isize) = a01;
                    *_pTmp0.offset((4 * x + 3) as isize) = a11;
                }
                syncL1();

                let mut cIdx = 0;
                for idxN in (xOffset * 2..blkN).step_by(xStep * 2) {
                    let iN = idxN + offsetN;
                    if iN >= N {
                        continue;
                    }

                    let mut a00j: f64;
                    let mut a01j: f64;
                    let mut a10j: f64;
                    let mut a11j: f64;
                    let mut a00k = get_zero();
                    let mut a01k = get_zero();
                    let mut a10k = get_zero();
                    let mut a11k = get_zero();

                    let mut b00j: f64;
                    let mut b01j: f64;
                    let mut b10j: f64;
                    let mut b11j: f64;
                    let mut b00k = get_zero();
                    let mut b01k = get_zero();
                    let mut b10k = get_zero();
                    let mut b11k = get_zero();

                    let idxB0 = iK + iN * LDB;

                    let mut pB0 = B.offset(idxB0 as isize);
                    let mut pB1 = B.offset((idxB0 + LDB) as isize);
                    let mut pTmp0 = _pTmp0;

                    macro_rules! do_2x2_ldd {
    ($i: expr) => (
        do_mad_ax_ldd_px!      (0, a00k, b00k, b00j, pB0,   (0 + ($i + 0) * 2) << 3);
        do_mad_ax_ldd_px!      (0, a01k, b01k, b01j, pB0,   (1 + ($i + 0) * 2) << 3);
        do_mad_ax_ldd_px!      (1, a10k, b00k, b10j, pB1,   (0 + ($i + 0) * 2) << 3);
        do_mad_ax_ldd_px_ct!   (1, a11k, b01k, b11j, pB1,   (1 + ($i + 0) * 2) << 3);
        do_mad_ax_ldd_px_local!(2, a00k, b10k, a00j, pTmp0, (0 + ($i + 0) * 4) << 3);
        do_mad_ax_ldd_px_local!(2, a01k, b11k, a10j, pTmp0, (1 + ($i + 0) * 4) << 3);
        do_mad_ax_ldd_px_local!(3, a10k, b10k, a01j, pTmp0, (2 + ($i + 0) * 4) << 3);
        do_mad_ax_ldd_px_local!(3, a11k, b11k, a11j, pTmp0, (3 + ($i + 0) * 4) << 3);

        do_mad_ax_ldd_px!      (0, a00j, b00j, b00k, pB0,   (0 + ($i + 1) * 2) << 3);
        do_mad_ax_ldd_px!      (0, a01j, b01j, b01k, pB0,   (1 + ($i + 1) * 2) << 3);
        do_mad_ax_ldd_px!      (1, a10j, b00j, b10k, pB1,   (0 + ($i + 1) * 2) << 3);
        do_mad_ax_ldd_px_ct!   (1, a11j, b01j, b11k, pB1,   (1 + ($i + 1) * 2) << 3);
        do_mad_ax_ldd_px_local!(2, a00j, b10j, a00k, pTmp0, (0 + ($i + 1) * 4) << 3);
        do_mad_ax_ldd_px_local!(2, a01j, b11j, a10k, pTmp0, (1 + ($i + 1) * 4) << 3);
        do_mad_ax_ldd_px_local!(3, a10j, b10j, a01k, pTmp0, (2 + ($i + 1) * 4) << 3);
        do_mad_ax_ldd_px_local!(3, a11j, b11j, a11k, pTmp0, (3 + ($i + 1) * 4) << 3);
    )
}
                    set_ax!("a0", get_zero());
                    set_ax!("a1", get_zero());
                    set_ax!("a2", get_zero());
                    set_ax!("a3", get_zero());

                    for _ in (0..bK).step_by(4 * 2 * 2 * 2) {
                        do_2x2_ldd!(0);
                        do_2x2_ldd!(2);
                        do_2x2_ldd!(4);
                        do_2x2_ldd!(6);
                        do_2x2_ldd!(8);
                        do_2x2_ldd!(10);
                        do_2x2_ldd!(12);
                        do_2x2_ldd!(14);

                        pB0 = pB0.offset(32);
                        pB1 = pB1.offset(32);
                        pTmp0 = pTmp0.offset(64);
                    }

                    do_mad_ax!(0, a00k, b00k);
                    do_mad_ax!(0, a01k, b01k);
                    do_mad_ax!(1, a10k, b00k);
                    do_mad_ax!(1, a11k, b01k);
                    do_mad_ax!(2, a00k, b10k);
                    do_mad_ax!(2, a01k, b11k);
                    do_mad_ax!(3, a10k, b10k);
                    do_mad_ax!(3, a11k, b11k);

                    a00k = tmpC[cIdx + 0];
                    a01k = tmpC[cIdx + 1];
                    a10k = tmpC[cIdx + 2];
                    a11k = tmpC[cIdx + 3];

                    b00k = add_ax!("a0", a00k);
                    b01k = add_ax!("a1", a01k);
                    b10k = add_ax!("a2", a10k);
                    b11k = add_ax!("a3", a11k);

                    tmpC[cIdx + 0] = b00k;
                    tmpC[cIdx + 1] = b01k;
                    tmpC[cIdx + 2] = b10k;
                    tmpC[cIdx + 3] = b11k;

                    if iK + bK >= K {
                        let idx00 = iM + iN * LDC;
                        let idx01 = idx00 + 1;
                        let idx10 = idx00 + LDC;
                        let idx11 = idx10 + 1;

                        *C.offset(idx00 as isize) = alpha * b00k + beta * *C.offset(idx00 as isize);
                        *C.offset(idx01 as isize) = alpha * b01k + beta * *C.offset(idx01 as isize);
                        *C.offset(idx10 as isize) = alpha * b10k + beta * *C.offset(idx10 as isize);
                        *C.offset(idx11 as isize) = alpha * b11k + beta * *C.offset(idx11 as isize);
                    }

                    chgthread();
                    cIdx += 4;
                }
            }
        }
    }
    flush();
}
```

ホストコードを書いて実行します。


```c++
    ...
    cl::Kernel k(program, "dgemm");
    clExtSetPerThreadStackSize(k(), 1024);

    auto dgemm = cl::make_kernel<
        int32_t, int32_t, int32_t,
        double, cl::Buffer, int32_t, cl::Buffer, int32_t,
        double, cl::Buffer, int32_t>(k);

    size_t n = 2048;
    size_t pad = 384;
    size_t stride = n + pad;

    vector<double> a(n * stride);
    vector<double> b(n * stride);
    vector<double> c(n * stride);

    mt19937 mt;
    uniform_real_distribution<double> dist(-1, 1);
    for (size_t i = 0; i < n * stride; i++) {
        a[i] = dist(mt);
        b[i] = dist(mt);
        c[i] = dist(mt);
    }
    cl::Buffer abuf(a.begin(), a.end(), false);
    cl::Buffer bbuf(b.begin(), b.end(), false);
    cl::Buffer cbuf(c.begin(), c.end(), false);

    auto start = chrono::system_clock::now();
    dgemm(cl::EnqueueArgs(8192), n, n, n, -1, abuf, stride, bbuf, stride, 1, cbuf, stride).wait();
    auto end = chrono::system_clock::now();

    double elapsed = chrono::duration_cast<chrono::duration<double>>(end-start).count();
    double flops = 2.0 * n * n * n / elapsed;

    cout << "elapsed: " << elapsed << " seconds, " << flops / 1e9 << " GFlops" << endl;

    vector<double> cret(n * stride);
    enqueueReadBuffer(cbuf, true, 0, sizeof(double) * n, &cret[0]);

    gemm_naive(n, n, n, -1, a, stride, b, stride, 1, c, stride);

    double err = 0;
    for (size_t i = 0; i < c.size(); i++) {
        err += abs(cret[i] - c[i]);
    }

    cout << "err: " << err << endl;
    ...
}
```

ここでは行列サイズ2048×2048の乗算を行っています。行われる浮動小数点演算の数は、行列サイズを`N`とすると、`2*N^3`回なので、これを所要時間で割れば実効性能が得られます。

```
$ ./host
elapsed: 0.0166524 seconds, 1031.67 GFlops
err: 7.1111e-08
```

2048×2048の行列の乗算にかかった時間がおよそ16.65ミリ秒ほどで、実効性能は1031.67GFlopsと出ました。定格でのPEZY-SCの倍精度演算のピーク値が1.5TFlopsですので、これはなかなか悪くない値です。実際ベースにしたC++版のコードでも1.1TFlopsに達しない程度ですので、Rust化による性能低下はごくわずか、あるいはRust版のコードもきちんと生成されるアセンブリコードを見てチューニングすれば、ほとんどないものとして扱えるようになるのではないかと思います。

# まとめ・所感

というわけで、RustのコードをPEZY-SCというプロセッサーで動かすことに成功しました。

Rustの処理系はバックエンドにLLVMを用いており、PEZY-SCの処理系はLLVMのバックエンドとして実装されているので、理屈の上では、これらを繋いでやるだけでRustのコードがPEZY-SCになるはずで、実際にやってみたところほとんどその理屈の通りにコンパイルして実行することが出来ました。

理屈の上では上手く行くはずのものでも、実際にすんなりと動いてくれたことは、ある意味では予想に反していたというか、Rustって素晴らしいという認識を新たにしました。

そういうわけで、皆さんもRustをいろいろな環境で動かして遊んでみてください。組み込み等でもRustのようなリッチな型を持つ言語を使えると言うことは、きっと新しい可能性をもたらしてくれるはずです。
