---
title: "Haskellで定理証明"
date: 2017-12-20T01:52:01+09:00
tags: ["haskell", "定理証明"]
---

```haskell
{-# LANGUAGE RankNTypes #-}

-- [定義]
-- Not
newtype Not p = Not (forall q. p -> q)

-- [公理]
-- 排中律
classic :: Either p (n p)
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
commutativeLawOr (Left  p) = Right p
commutativeLawOr (Right q) = Left  q

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
deMorgan (np, nq) = Not (\pq ->
    case pq of
        Left  p -> bomb p np
        Right q -> bomb q nq )

-- 二重否定法則
pnnp :: p -> Not (Not p)
pnnp p = Not $ bomb p

nnpp :: Not (Not p) -> p
nnpp nnp = case classic of
    Left  p  -> p
    Right np -> bomb np nnp

-- 対偶
contraposition1 :: (p -> q) -> (Not q -> Not p)
contraposition1 h nq = Not (\p -> bomb (h p) nq)

contraposition2 :: (Not q -> Not p) -> (p -> q)
contraposition2 h p = nnpp $ (contraposition1 h) (pnnp p)

--いろいろ
implyToOr :: (p -> q) -> Either (n p) q
implyToOr pq = case classic of
    Left  r  -> Right $ pq r
    Right nr -> Left nr

-- パースの法則
peirce_lemma1 :: ((p -> q) -> p) -> ((Not q -> Not p) -> p)
peirce_lemma1 x y = x (contraposition2 y)

peirce_lemma2 :: (Not p -> p) -> p
peirce_lemma2 x = either id x classic

peirce :: ((p -> q) -> p) -> p
peirce h =
    peirce_lemma2
    (
        const
        $
        either
        (h . const)
        (peirce_lemma2 . associativeLaw (peirce_lemma1 h))
        classic
    )

-- なんか
prop1 :: (forall p. ((p -> q) -> q)) -> ((p -> q) -> p) -> p
prop1 h h0 = h0 $ const $ h id

main :: IO ()
main = return ()
```