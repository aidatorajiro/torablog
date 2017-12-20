---
title: "Haskellで定理証明"
date: 2017-12-20T01:52:01+09:00
tags: ["haskell", "定理証明"]
categories: ["haskell"]
---

Haskellで定理証明もどき、やってみた

{{< highlight haskell >}}
{-# LANGUAGE RankNTypes #-}

-- [定義]
-- Not
newtype Not p = Not (forall q. p -> q)

-- [公理]
-- 排中律
classic :: Either p (Not p)
classic = undefined

-- [証明]
-- 三段論法
syllogism :: (p -> q) -> (q -> r) -> (p -> r)
syllogism h1 h2 = h2 . h1

-- 交換法則（かつ）
commutativeLawAnd :: (p, q) -> (q, p)
commutativeLawAnd (p, q) = (q, p)

-- 交換法則（または）
commutativeLawOr :: Either p q -> Either q p
commutativeLawOr = either Right Left

-- 結合法則
associativeLaw :: ((p -> q) -> r) -> (p -> (q -> r))
associativeLaw h = const $ h . const

-- P -> Pでない -> Q
bomb :: p -> Not p -> q
bomb p (Not pq) = pq p

-- (Pかつ(Pでない))ならばQ
bombAnd :: (p, Not p) -> q
bombAnd (p, np) = bomb p np

-- ド・モルガンの法則
deMorgan :: (Not p, Not q) -> Not (Either p q)
deMorgan (np, nq) = Not $ either (`bomb` np) (`bomb` nq)

-- 二重否定法則
pnnp :: p -> Not (Not p)
pnnp p = Not $ bomb p

nnpp :: Not (Not p) -> p
nnpp nnp = either id (`bomb` nnp) classic

-- 対偶
contraposition1 :: (p -> q) -> (Not q -> Not p)
contraposition1 h nq = Not (\p -> bomb (h p) nq)

contraposition2 :: (Not q -> Not p) -> (p -> q)
contraposition2 h p = nnpp $ contraposition1 h (pnnp p)

--いろいろ
implyToOr :: (p -> q) -> Either (Not p) q
implyToOr pq = either (Right . pq) Left classic

-- パースの法則
peirce :: ((p -> q) -> p) -> p
peirce h = either id (\(Not px) -> h px) classic

-- なんか
prop1 :: (forall p. ((p -> q) -> q)) -> ((p -> q) -> p) -> p
prop1 h h0 = h0 $ const $ h id

main :: IO ()
main = return ()
{{< /highlight >}}

### 解説

{{< highlight haskell >}}
-- [定義]
-- Not
newtype Not p = Not (forall q. p -> q)
{{< /highlight >}}
Haskellの世界には否定は存在しないので、ここで定義している。

```forall q. p -> q```というのは「pからなんでも出てくるような関数」の型なので、pが存在してしまうと、そのような関数は存在できなくなる。つまり、```forall q. p -> q```という関数が存在するということは、pでないということだと直感的には理解できる。

実際、この定義で後々の否定に関する定理も証明できるし、論理学的にも、$(\forall p. p \Rightarrow q) \Leftrightarrow (\lnot p)$である。

Haskellには否定の概念はないが、多相型(forall)があるので、否定を表現することができる。Haskellにはこんな調子で、何か有名な概念そのものは言語に実装されていないが、代わりにその概念を定義できるような機能が実装されている、というパターンが多い。モナドだって型クラスの機能を使ってHaskellで定義されているし、リストのデータ構造の定義もHaskellで書かれている。

{{< highlight haskell >}}
-- [公理]
-- 排中律
classic :: Either p (Not p)
classic = undefined
{{< /highlight >}}

これを公理と言っていいのかわからないが、公理である。

<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
<script type="text/x-mathjax-config">
MathJax.Hub.Config({
  tex2jax: {
    inlineMath: [['$','$'], ['\\(','\\)']],
    displayMath: [['$$','$$'], ['\[','\]']],
    processEscapes: true,
    processEnvironments: true,
    skipTags: ['script', 'noscript', 'style', 'textarea', 'pre'],
    TeX: { equationNumbers: { autoNumber: "AMS" },
         extensions: ["AMSmath.js", "AMSsymbols.js"] }
  }
});
</script>