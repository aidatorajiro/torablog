---
title: ReflexでSPA作ってみる
date: 2018-06-13T16:47:00+09:00
---

Haskell製のWebアプリケーションフレームワークの一つに、Reflexというものがある。今回はそれを使ってSPA(Single Page Application)を実装してみる。

## GHCJSとReflex

HaskellコンパイラのひとつであるGHCは、以下のような流れでプログラムを機械語に変換している。[^1]

![](/static/img/reflex-spa/HscPipe2.png)

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

たとえHaskellのような関数型言語であっても、外の世界が関係する部分は、どうしても手続き的な表現をする他ない。ユーザの入力を待つ -> `x`を更新する -> `y`を更新する、といったように。しかしFRPでは、ユーザの入力や時間経過が関係する部分でさえ、それらを"加工"して、定義的=関数的=Functionalに書ける。もちろん、内部では手続き的な表現でプログラムが動いているのだけれど(最終的にはJavascriptや機械語に変換されるから)、Reflexはそれを関数的に記述できるインターフェースを用意してくれている。

FRPのもっとわかりやすい例として、Excelが挙げられる。Excelでは、あるセルの計算式に関係する部分を変更すると、自動的にセルの内容が再計算され、更新される。たとえば、A11をA1からA10までの数値の和(`A11 = SUM(A0:A10)`)としてやると、A1からA10までの数値をユーザが変更するたびに、A11の内容が再計算される。

## ReflexとReflex-DOMの仕組み

ReflexとReflex-DOMの仕組みは、<https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md>でとても詳しく説明されている。以下の3つの図は、全てこのページから引用している。

*注意: 以下は、Reflexバージョン0.5、Reflex-DOMバージョン0.4に関する内容。*

まずはHaskell / Reflexの型・関数の書き方を説明する。

- 型の名前は必ず大文字から始まらないといけない。

- 型を小文字から書き始めた場合、どんな型でも入ると言う意味になる。ただし、同じ文字には同じ型が入る。例：恒等写像`id :: a -> a`

- 関数は`関数名 :: 型`で表す。関数の型は`引数1の型 -> 引数2の型 -> ... -> 引数nの型 -> 返り値の型`で表す。例えば、整数が引数で整数が出力の関数fは、`f :: Int -> Int`で表す。

- `Event Int`のような形の型は、内部に`Int`型の値を保持している型になる。`Event`だけでは型になり得ず、内部にどんな型が存在しているかも指定しなければいけない。Javaのジェネリクスみたいなやつ。

- 関数への入力は、`関数名 引数1 ... 引数n`で表す。

- Reflexにおいて、文字列は`Text`型をもつ。というか、`Text`型じゃないと受け付けてくれない。

ReflexとReflex-DOMでは、4つの重要な概念が登場する。Event、Behavior、Dynamic、Widgetだ。

- Event
  
  Eventは、値が変わった瞬間と、その値を伝えるもの。  
  たとえば、マウスクリックだったら、XY座標と、クリックされた瞬間がわかる。キーボード入力やマウスクリックなどのユーザ入力は全てEventだ。以下のような関数がある。

  - `domEvent Click el` - HTML要素のクリックイベント

  Eventの型は`Event t a`で表される。aが内部に保持される値の型(上の例だとXY座標)。tはおまじない的なもので、プログラムする際には関係ない。実はよくわからない。

  ![](/static/img/reflex-spa/event.png)

- Behavior
  
  Behaviorは、通常の変数のこと。Behaviorはいかなる時点でも必ず何らかの値を持つが、いつ値が変わったのかを知ることはできない。

  Eventに初期値を与えると、Behaviorを作ることができる。  
  `hold :: a -> Event t a -> Behavior t a`

  BehaviorからEventを作ることは、振る舞いがわからないので不可能。

  ![](/static/img/reflex-spa/behavior.png)

- Dynamic
  
  Dynamicは、EventとBehaviorの組み合わせ。つまり、変化する値と、それが変更された瞬間の両方が格納されている。

  DynamicからはEventやBehaviorを取り出すことができる。  
  `current :: Dynamic t a -> Behavior t a`  
  `updated :: Dynamic t a -> Event t a`

  Eventに初期値を与えると、Dynamicを作ることができる。  
  `holdDyn :: a -> Event t a -> Dynamic t a`

  BehaviorからDynamicを作ることは、振る舞いがわからないので不可能。

  ![](/static/img/reflex-spa/dynamic.png)

- Widget
  
  ページを構成する要素の事。Dynamicの値に応じて内容を動的に変化させることができる。これを作成できる関数は、

  - `el :: Text -> Widget t a -> Widget t a` - HTMLタグ作成  
  - `text :: Text -> Widget t a` - 固定テキスト作成  
  - `dynText :: Dynamic Text -> Widget t a` - 可変テキスト作成
  - `button :: Text -> Widget t (Event t ())` - ボタン作成 / クリックイベントが付いてくる
  
  などがある。aはどんな型でも良い。elがWidgetを消費してWidgetを生成しているのは、タグの中の内容を指定させるため。
  
  例えば、`x :: Dynamic Text`に対し、`el 'span' (dynText x)`は、内容がxのspan要素のこと。xの値が`'test'`の時、生成されるHTMLは`<span>test</span>`になる。xの値が変化するごとに、span要素の内容も連動して変化する。

  ボタンを作成して、そのクリックイベントを取得し、それをカウントしてDynamicを作り、最終的にspan要素の中に表示するWidgetの例を示す。

  ```haskell
  (\ev -> el 'span' (dynText (count ev))) =<< button 'click me'
  ```

## ルーティングの実装

今回は、一つのHTMLファイルにサーバへの全通信を繋げて、HTML内のJavascriptが接続パス等の情報をもとにサイトのルーティングをするという仕組みを作る。この際、複数のページが存在し、それぞれのページから他のページへのリンクを張ることができる。

そもそも、ルーティングとはなんだろう？

再帰させなければいけない

再帰で一旦書く

mdo記法つかう

では、ルーティングという概念の数学的（？）構造を理解したところで、実際にそれをReflexのコードに落とし込んでみる。まずは最初に、結論となるコードを記す。

```haskell
import Reflex.Dom
import Reflex.Dom.Location ( getLocationPath )

main :: IO ()
main = do
  init_loc <- getLocationPath
  mainWidget $ mdo
    ee <- dyn $ (\l -> pushState l >> router l) <$> loc
    be <- hold never ee
    loc <- holdDyn init_loc (switch be)
    return ()
```

ここで`router l`関数は、パスlに対応するWidgetを生成する。`pushState l`関数は、HTML5 History APIのpushState関数を呼び、ブラウザの履歴にパスlを追加する。

coincidenceで詰んだ -> hold & switchを使おう

## 感想
Haskellやその周辺のフレームワークは数学的（？）思考を強いてくるから非常にめんどくさい　でもそこがいい！

## 参考文献

https://github.com/reflex-frp/reflex  
https://github.com/reflex-frp/reflex-dom  
https://github.com/reflex-frp/reflex/blob/develop/Quickref.md  
https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md

[^1]: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/HscPipe
[^2]: https://github.com/ghcjs/ghcjs/wiki/Architecture
