---
title: Electron + node-ffi
date: 2017-12-27T12:31:00+09:00
tags: ["Electron", "Node", "黒魔術"]
draft: true
---


## 環境


## 内容
CのライブラリをNode側で呼べる、node-ffiというライブラリをElectronから叩いた時に詰まったことなどをメモ。

### NODE_MODULE_VERSIONで詰まる

早速、npmでnode-ffiを入れてみて、それをElectronからrequireしてみたところ、NODE_MODULE_VERSIONが合ってない！！！と怒られた。

なんじゃそれと思って調べると、どうやらnpmを動かしたnodeの

### Webpack編

Node.js内でCのコードを動かすには、.nodeという拡張子を持つファイルに一旦コンパイルしてから、それをnode側でrequireします。node-ffiもこの仕組みを使っています。

Webpackで