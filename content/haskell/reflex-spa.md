---
title: ReflexでSPA作ってみる
date: 2018-06-13T16:47:00+09:00
draft: true
---

Haskell製のWebフレームワークの一つに、Reflexというものがある。今回はそれを使ってSPA(Single Page Application)を作ってみる。

SPAというのは、一つのHTMLファイルにサーバへの全通信を繋げて、HTML内のJavascriptが接続パス等の情報をもとにサイトのルーティングをするという仕組みのことだ。つまり、ルーティングも含めてJavascriptがやってしまおうというわけだ。

## GHCJSとReflex

Wikiによると、HaskellコンパイラのひとつであるGHCは、以下のような仕組みでプログラムを機械語に変換している。

<https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/HscPipe>より抜粋:

![](/static/HscPipe2.png)

これを見ると、処理の最後の方ではSTG形式 -> Cmm形式 -> 機械語の順で変換していることがわかるが、このSTGという中間言語になったところで、それをJavascriptに翻訳するコンパイラがGHCJSだ。[^1]

さらに、HaskellのFRP(Functional Reflective Programming)ライブラリとしてreflexというものがあり、GHCJSに対応してDOM、すなわちHTMLの動的な操作などができるようになったのがreflex-domだ。今回はこのreflex-domを用いて、SPAを構築する。

このように、フロントエンド界の他の様々なライブラリ・フレームワーク群と同様、かなりソフトウェア同士の関係性が複雑になっている...

## Reflexの仕組み、難解！

まずは、reflexを用いてどのようにコーディングするのかを解説する。

## SPAを作る

SPAは、一つのHTMLファイルにサーバへの全通信を繋げて、HTML内のJavascriptが接続パス等の情報をもとにサイトのルーティングをするという仕組み。

そもそも、ルーティングとはなんだろう？

再帰させなければいけない

再帰で一旦書く

mdo記法つかう

では、ルーティングという概念の数学的構造を理解したところで、実際にそれをReflexのコードに落とし込んでみる。まずは最初に、結論となるコードを記す。



なんとこのコードを完成させるまでに、1日半以上かかった。まず型を合わせるのに1日、その後、型は合ってるのに動かないという現象が発生した。ここで少し読みづらくなるかもしれないが、私がこのコードに至るまでに、どのように試行錯誤してきたのかを話したいと思う。

coincidenceで詰んだ -> hold & switchを使おう

## 感想
Haskellやその周辺のフレームワークは我々に数学的思考を強いてくるから非常にめんどくさい　でもそこがいい！

[^1]: https://github.com/ghcjs/ghcjs/wiki/Architecture