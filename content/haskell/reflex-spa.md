---
title: ReflexでSPA作ってみる
date: 2018-06-13T16:47:00+09:00
---

Haskell製のWebアプリケーションフレームワークの一つに、Reflexというものがある。今回はそれを使ってSPA(Single Page Application)を実装してみる。

今回は、一つのHTMLファイルにサーバへの全通信を繋げて、Javascriptが接続先パスをもとにサイトのルーティングをし、ページのレンダリングをする仕組みを実装する。

## GHCJSとReflex

HaskellコンパイラのひとつであるGHCは、以下のような流れでプログラムを機械語に変換している。[^1]

![](/img/reflex-spa/HscPipe2.png)

これを見ると、処理の最後の方ではSTG形式 -> Cmm形式 -> 機械語の順で変換していることがわかるが、このSTGという中間言語になったところで、それをJavascriptに翻訳するコンパイラがGHCJSだ。[^2]

さらに、HaskellのFRPフレームワークとしてReflexというものがあり、それをGHCJSに対応させてDOM、すなわちHTMLの動的な操作ができるようになったのがReflex-DOMだ。今回はこのReflexとReflex-DOMを用いて、SPAを構築する。

フロントエンドのフレームワークにはよくあること（？）かもしれないけど、かなりソフトウェア同士の関係が複雑になっている...

## FRPとは？

ReflexはFRP(Functional Reactive Programming)フレームワークである。

簡単に言うと、FRPとは、ある一つの値が変わると、それに関係した=それによって定義された他の値も自動的に変わるということだ。

FRPの観念的な例として以下のコードを挙げる。(これはReflexのコードではない。あくまで観念的な擬似コード。)
```javascript
x = 8
y = x + 1
x += 1
```

このプログラムを普通に解釈してみる。

まず最初に`x`に`8`が代入される。`x + 1`が計算され、それが`y`に代入される。その後`x`が`1`加算されるが、`y`には影響がない。最終的には、`x = 9`、`y = 9`になる。

では次に、全く同じコードを、FRP的に解釈してみよう。上記のコードはこのように解釈される。

まず最初に`x`を`8`と定義する。`y`を`x + 1`と定義する。`x`に`1`加算されるが、ここでさきほど`y`を`x`で定義したので、`y`の値を現在の`x`の値を用いて再計算する。最終的には、`x = 9`、`y = 10`になる。

FRPの重要な点は、式がそのまま恒真な命題となることだ。上記のコードにおいて、普通な解釈では`y = x + 1`は2行目までは成り立つが、3行目からは成り立たない。しかしFRP的な考え方では、`y = x + 1`はどんな場合でも成り立つ。そのように`y`が定義され、値を自動で更新するプログラムが裏で動いているからだ。

本当は、FRPにおいて上記コード内の`x += 1`という部分は、外の世界(ユーザの入力, 乱数, 時刻etc)に関係する部分である。なぜなら、そういった不確定要素がなければ、初めから全ての変数の値が計算できてしまい、変数を更新する必要がないからである。たとえば、ボタンAを押すたび`x += 1`、1秒ごとに`x += 1`と言った具合だ。すなわち、`x`は`8 + ボタンAの押された回数`や`8 + 経過秒数`として定義される。こう考えると、FRPとはすなわち外の世界によって変化する値の定義(の集合)であることがわかる。ある意味では、外の世界が一つのプリミティブな変数であり、それを"加工"し、外の世界が関係する変数を作り上げることがFRPであるとも言える。Reflexの場合、最終的に生成するべきものは動的に変化するページである。それを、外の世界を"加工"して定義する。イメージ的にはそんな感じだ。

たとえHaskellのような関数型言語であっても、外の世界が関係する部分は、どうしても手続き的な表現をする他ない。ユーザの入力を待つ -> `x`を更新する -> `y`を更新する、といったように。しかしFRPでは、ユーザの入力や時間経過が関係する部分でさえ、それらを"加工"して、定義的=関数的=Functionalに書ける。もちろん、内部では手続き的な表現でプログラムが動いているのだけれど、Reflexはそれを関数的に記述できるインターフェースを用意してくれている。

FRPのもっとわかりやすい例として、Excelが挙げられる。Excelでは、あるセルの計算式に関係する部分を変更すると、自動的にセルの内容が再計算され、更新される。たとえば、A11をA1からA10までの数値の和(`A11 = SUM(A0:A10)`)としてやると、A1からA10までの数値をユーザが変更するたびに、A11の内容が再計算される。

## ReflexとReflex-DOMの環境構築
このサイトに詳しい手順が書かれている。  
<https://github.com/reflex-frp/reflex-platform>

Reflexのソースコード`your-source-file.hs`を用意した上で、以下のコマンドを順に実行する。(Linux と MacOS のみ対応)

1. `git clone https://github.com/reflex-frp/reflex-platform`
2. `cd reflex-platform`
3. `./try-reflex` (めっちゃ時間かかる)
4. `nix-shell`
5. `ghcjs --make your-source-file.hs`

すると、ディレクトリ`your-source-file.jsexe`が生成され、その中にindex.htmlやJavascriptがある。

ただし、今回はSPAを作るので、別途サーバを立てなければいけない。

`your-source-file.jsexe`内で`npm install express`を実行したのち、`server.js`と言う名前で以下のNode.jsプログラムを作成し、`node server.js`を実行する。http://localhost:8080/ に行けば、ページが動いていることが確認できる。

```javascript
const express = require('express');
const path = require('path');
const port = process.env.PORT || 8080;
const app = express();

app.get('/:filename.js', function (request, response) {
  response.sendFile(path.join(__dirname, request.params.filename + ".js"));
});

app.get('*', function (request, response) {
  response.sendFile(path.join(__dirname, 'index.html'));
});

app.listen(port);
console.log("server started on port " + port);
```

## ReflexとReflex-DOMの仕組み

*注意: Reflexバージョン0.5、Reflex-DOMバージョン0.4に関する内容。*

まずはHaskell / Reflexの型・関数の書き方を説明する。

- 型の名前は必ず大文字から始まらないといけない。

- 型を小文字から書き始めた場合、どんな型でも入ると言う意味になる。ただし、同じ文字には同じ型が入る。例：恒等写像`id :: a -> a`{.haskell}

- 関数は、1行目に`関数名 :: 引数1の型 -> ... -> 引数nの型 -> 返り値の型`、2行目に`関数名 引数1 ... 引数n = プログラム`で定義する。引数を二乗する関数の例:
  ```haskell
  func :: Int -> Int
  func n = n * n
  ```
  「funcは`Int -> Int`{.haskell}型である」「`Int -> Int`{.haskell}型の関数funcが存在する」という意味で、`func :: Int -> Int`{.haskell}と書く場合がある。

- `Event Int`{.haskell}のような、大文字から始まる語が二語以上続く型では、内部にどんな型の値があるか指定することができる。Javaのジェネリクスみたいなやつ。

- 関数への入力は、`関数名 引数1 ... 引数n`で表す。

- 関数名が記号の場合、入力を`引数1 * 引数2`のように書くことができる。この時、`* :: 引数1の型 -> 引数2の型 -> 出力の型`となる。

- Reflexにおいて、文字列は`Text`{.haskell}型をもつ。本当は`String`{.haskell}型が標準なのだが、`Text`{.haskell}型じゃないと受け付けてくれない。

- 無名関数は、`(\引数1 ... 引数n -> プログラム)`で表す。

- 何もデータがない型(nullのようなもの)は、空タプル`()`で表す。

ReflexとReflex-DOMでは、4つの重要な概念が登場する。Event、Behavior、Dynamic、Widgetだ。

### Event

Eventは、ある特定の時間に発火し、何らかの値を伝える。  
たとえば、キー入力だったら、キーが押された瞬間に、キーコードを伝える。キーボード入力やマウスクリックなどは全てEventだ。  

以下のような関数がある。

- `domEvent Click el :: Event t ()`{.haskell} - HTML要素のクリックを伝える。クリック時に発火するが、値を持たない。
- `domEvent Keypress el :: Event t Int`{.haskell} - HTML要素内でキーが押された時に発火する。キーコードが値となる。

Eventの型は`Event t a`で表される。aが内部に保持される値の型(上の例だとキーコード)。tはおまじない的なもので、プログラムする際には関係ない。

図としてイメージすると、以下のような感じになる。ある特定の時間において、Eventが発火し、その時に同時に値が伝えられる。

![](/img/reflex-spa/event.png)  
<small>(https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md から引用。(c) Hans Roland Senn)</small>

### Behavior

Behaviorは、通常の変数のこと。Behaviorはいかなる時点でも必ず何らかの値を持つが、いつ値が変わったのかを知ることはできない。

以下のような関数がある。

- `hold :: a -> Event t a -> Widget t (Behavior t a)`{.haskell}  - Eventに初期値を与えることで、Behaviorを作る。ただし、後述のWidgetに包まれて帰ってくる。

BehaviorからEventを作ることは、Behaviorの振る舞いがわからないので不可能。

![](/img/reflex-spa/behavior.png)  
<small>(https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md から引用。(c) Hans Roland Senn)</small>

### Dynamic

Dynamicは、EventとBehaviorの組み合わせ。つまり、変化する値と、それが変更された瞬間の両方が格納されている。

以下のような関数がある。

- `current :: Dynamic t a -> Behavior t a`{.haskell}  - DynamicからBehaviorを取り出す。
- `updated :: Dynamic t a -> Event t a`{.haskell}  - DynamicからEventを取り出す。
- `holdDyn :: a -> Event t a -> Widget t (Dynamic t a)`{.haskell}  - Eventに初期値を与えることで、Dynamicを作る。ただし、後述のWidgetに包まれて帰ってくる。
- `count :: Event t a -> Widget t (Dynamic t Int)`{.haskell}  - Eventが発生した回数を数え、`Int`が格納されたDynamicを作成する。ただし、後述のWidgetに包まれて帰ってくる。
- `<$> :: (a -> b) -> (Dynamic a -> Dynamic b)`{.haskell}  - 超便利関数。任意の関数を、入力Dynamic出力Dynamicの関数に変化させることができる。じつはDynamicの親分にFunctorというものがあって、そこに入っている関数。

BehaviorからDynamicを作ることは、Behaviorの振る舞いがわからないので不可能。

![](/img/reflex-spa/dynamic.png)  
<small>(https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md から引用。(c) Hans Roland Senn)</small>

### Widget

ページを構成する要素の事。Dynamicの値に応じて内容を動的に変化させることができる。これを作成できる関数は、

- `el :: Text -> Widget t a -> Widget t a`{.haskell}  - HTMLタグ作成。Widgetを消費してWidgetを生成しているのは、タグの中の内容を指定させるため。
- `text :: Text -> Widget t ()`{.haskell}  - 固定テキスト作成
- `dynText :: Dynamic Text -> Widget t ()`{.haskell}  - 可変テキスト作成
- `button :: Text -> Widget t (Event t ())`{.haskell}  - ボタン作成。クリックイベントが付いてくる。

などがある。また、DynamicやBehaviorを生成する一部の関数は、`Widget t (Dynamic t a)`{.haskell}の形で値を返す。

例えば、`x :: Dynamic Text`{.haskell} に対し、`el "span" (dynText x)`{.haskell} は、内容がxのspan要素のこと。xの値が`"test"`{.haskell}の時、生成されるHTMLは`<span>test</span>`{.html}になる。xの値が変化するごとに、span要素の内容も連動して変化する。

do記法(Widgetの親分にMonadというものがあって、それについてくる機能)を用いると、複数のWidgetをつなげたり、`<-`を使ってWidget内部の値を"取り出す"ことができる。これは、先ほどのholdやholdDynやbuttonなどから作られる、Widgetに包まれた値を処理するのにとても役に立つ。do記法の最後の行は必ずWidgetでなければならない。そして、do記法で作られたWidgetの型はその最後の行の型と一致する。return関数を使うと、任意の値をWidgetに包むことができる。

#### span要素が三つ並んでいるWidgetの例

```haskell
widget1 :: Widget t ()
widget1 = do
  el "span" (text "テキスト1")
  el "span" (text "テキスト2")
  el "span" (text "テキスト3")
  return ()
```

見ての通り、do記法を使うと3つのelを1つに合成することができる。最後、`return ()`としているのは、widget1の型を`Widget t ()`{.haskell}にするため。

#### ボタンを押した回数を表示するWidgetの例

```haskell
widget2 :: Widget t ()
widget2 = do
  ev <- button "click me"
  dynText (count ev)
  return ()
```

`button "click me"`{.haskell} の型は`Widget t (Event t ())`{.haskell} だけど、`<-`という記号を用いると、Widgetの中の値を"取り出す"ことができる。つまり、evの型は`Event t ()`{.haskell}になる。その後これを、count関数に通してDynamic化し、dynTextに通してWidget化している。



## Reflexソースコードの書き方

先ほどの、「ボタンを押した回数を表示するWidget」を、実際にソースコードにすると、こうなる。

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

widget2 :: Widget t ()
widget2 = do
  ev <- button "click me"
  dynText (count ev)
  return ()

main :: IO ()
main = mainWidget widget2
```

1行目でOverloadedStrings言語拡張を導入して、自動的に文字列がText型に変換されるようにしている。

3行目でReflexをインポートしている。

Haskellプログラムのエントリーポイントはmain関数だ。mainWidgetを使うと、`Widget t ()`{.haskell}をmain関数の型`IO ()`{.haskell}に変換してくれる。

## ルーティングの実装

今回は、一つのHTMLファイルにサーバへの全通信を繋げて、HTML内のJavascriptが接続先パスをもとにサイトのルーティングをする仕組みを作る。この際、複数のページが存在し、それぞれのページから他のページへのリンクを張ることができる。

パスが変わると、パスの変わり方も変わる。それがリンクの構造だ。だからこそ、ページAからページBに、ページBからページAにリンクが貼ってある時、無限にAとBの間を往復できる。

Reflexには、そのような再帰的構造を上手く扱える関数が用意されている。

```haskell
mfix :: (a -> Widget t a) -> Widget t a
```

以下3つの仮定から、

- パスを格納するDynamicを`loc :: Dynamic t Text`{.haskell} として扱う。

- 「ページ」の型を`Widget t (Event t Text)`{.haskell} と定義する。これは、ページの内容と、さらにパスの変更、つまり次のページが何になるかを指し示すEventを保持している。

- パスを入れたら対応するページが出てくる関数`getPage :: Text -> Widget t (Event t Text)`{.haskell} があるものとする。

まずは、先ほど紹介した超便利関数`<$>`を使って、動的に変化するWidgetを作る。

```haskell
getPage <$> loc :: Dynamic t (Widget t (Event t Text))
```

なかなか複雑な型になってきた。この型は、「動的に変化する、Text型のEventを格納したWidget」と言う意味になる。しかしこのままではDynamicに包まれたWidgetができるだけで、Widgetを作れない。最終的に作るべきものはWidgetだから、何とかして中身のWidgetを外に持ってくる必要がある。そこでReflex-DOMの`dyn`関数を使う。

```haskell
dyn :: Dynamic t (Widget t a) -> Widget t (Event t a)
```

これに、先ほどの`getPage <$> loc`を適用すると...

```haskell
dyn (getPage <$> loc) :: Widget t (Event t (Event t Text))
```

が出てくる。これで、Widgetを一番外に持ってくることができたが、ここでまた問題が発生する。Eventが二重になっている。今度はこれを一重にしなければならない。

外側のEventと内側のEventとの違いについて考えてみる。外側のEventは、dyn関数がDynamicをWidgetに変換する時についてきたものだ。しかし、今欲しいものは、次にどのページに遷移するかのEventである。だから、外側のEventは無視して、内側のEventを採用する必要がある。

この時、`dyn`関数が内側と外側を入れ替えるような心配はしなくて良い。なぜなら、`dyn`関数は **全ての型aに対し** `Dynamic t (Widget t a)`を`Widget t (Event t a)`に変換しているからだ。すなわち、`dyn`関数はaがどのような性質を持っているか全く知らない。付加されるEventは入力のDynamicやWidgetに関係しているかもしれないが、決してaには関係できない。だから、外側のEventは内側のEventと関係がない。このように、型から関数の振る舞いを予測できるのも、Haskellの醍醐味。

内側のEventだけを採用する方法はいくつかあるが、`hold`、`never`、`switch`を組み合わせた方法を用いる。

`hold`はEventに初期値を与えることでBehaviorを作る関数、`never`は絶対に発火しないイベント、`switch`は現在Behaviorに入っているEventが発火した時に発火するEventを作る関数だ。

- `hold :: a -> Event t a -> Widget t (Behavior t a)`{.haskell}

- `never :: Event t a`{.haskell}
  (絶対に発火しないので、値を必要をしない。だから中の値の型は何でもよくなる)

- `switch :: Behavior t (Event t a) -> Event t a`{.haskell}

このようにして、最終的に以下のような形で`loc`から対応するページを描画した上で、`Dynamic t Text`を求める関数が完成した。

```haskell
func :: Dynamic t Text -> Widget t (Dynamic t Text)
func loc = do
  eventOfEvent <- dyn (getPage <$> loc)
  behaviorOfEvent <- hold never eventOfEvent
  holdDyn "/" (switch behaviorOfEvent)
```

さて、ここでここ関数の型`Dynamic t Text -> Widget t (Dynamic t Text)`に注目する。じつはこの形がすごく重要なのだ。

`holdDyn`を用いて、EventからDynamicを作っている。さらに、先ほどまで定義が明らかにされていなかったlocに対して自分自身を指定している(再帰)。実はこれでもう、ルーティングは完成だ。

## 感想
Haskellやその周辺のフレームワークは数学的（？）思考を強いてくるから非常にめんどくさい　でもそこがいい！

## 参考文献
https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md  
https://github.com/reflex-frp/reflex  
https://github.com/reflex-frp/reflex-dom  
https://github.com/reflex-frp/reflex/blob/develop/Quickref.md  
https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md  
https://github.com/reflex-frp/reflex-dom-contrib/blob/master/src/Reflex/Dom/Contrib/Router.hs

[^1]: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/HscPipe

[^2]: https://github.com/ghcjs/ghcjs/wiki/Architecture
