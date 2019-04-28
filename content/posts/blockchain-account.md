---
title: p2pkh p2wpkh-nested-in-p2sh p2wpkh p2wsh
tags: ["Bitcoin"]
date: 2018-11-15T19:26:00+09:00
---

- if base64
  - p2pkh (20 bytes key hash) - starts with `1` (testnet: `n`/`m`)
  - p2sh (20 bytes script hash) - starts with `3` (testnet: `2`)
    - p2wpkh-nested-in-p2sh (`script hash = hash160(0014<20 byte key hash>)`)

- if bech32 - starts with `bc1` (testnet: `tb1`)
  - p2wpkh (20 bytes key hash)
  - p2wsh (32 bytes script hash)
