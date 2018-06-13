---
title: "Quantum Blockchain using entanglement in time" 読んでみた
date: 2018-06-13T16:48:00+09:00
draft: true
---

めっちゃおもしろそうな論文があったので読んでみた 
物理全くわからんので雰囲気 
以下、<https://arxiv.org/pdf/1804.05979.pdf>から抜粋／引用

## Abstract要約

> A conceptual design for a quantum blockchain is proposed. Our method involves encoding the blockchain into a temporal GHZ (Greenberger–Horne–Zeilinger) state of photons that do not simul- taneously coexist. It is shown that the entanglement in time, as opposed to an entanglement in space, provides the crucial quantum advantage. All the subcomponents of this system have already been shown to be experimentally realized. Perhaps more shockingly, our encoding procedure can be interpreted as non-classically influencing the past; hence this decentralized quantum blockchain can be viewed as a quantum networked time machine.

- 量子ブロックチェーンを提案するよ
- 同時に存在しない二量子系(時間的量子もつれ)にブロックチェーンをエンコードする
- 使われている技術は割とすでに実現済み
- エンコーディングに際して、non-classicallyに過去を書き換えている
- だから、タイムマシンと見ることもできる

量子もつれ - 二つの量子が同じ状態を共有している現象のこと。たとえば、量子Aを観測したときに1という値が出たとき、すぐさま量子Bの値が-1だと分かるような状況だったときに、二つの量子は"もつれている"という。普通、量子というものは確率的で、観測するまで値が分からないが、量子もつれの元では片方を観測するともう一方の値も確定する。

## 時間的量子もつれ

> In this Letter, we will propose a conceptual design for a quantum blockchain using entanglement in time. Non-classical correlations between temporally separated quantum systems have manifested itself through various physical settings; the particular case used in this work involves entanglement in time between photons that do not simultaneously coexist [22].

## 古典的ブロックチェーンに関する説明

> Classical blockchain: The aim of a blockchain is to have a single database of records about the past that every node in the network can agree on. Furthermore, it should not require a centralized management node. It will be helpful to construct a physical model to describe this classical information system, i.e., its kinematic and dynamic properties.
