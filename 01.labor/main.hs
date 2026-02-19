-- # 1. labor

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: Int -> Int -> Int
osszeg a b = a+b

kivon :: Double -> Double -> Double
kivon a b = a-b

szorz :: Int -> Int -> Int
szorz a b = a*b

oszt :: (Fractional a) -> a -> a
oszt a b = a/b

oszt2 :: (Integral a) -> a -> a
oszt2 a b = a div b

oszt3 :: (Integral a) -> a -> a
oszt3 a b = a `div`` b

osztmar :: (Integral a) -> a -> a
osztmar a b = mod a b
-- - egy első fokú egyenlet gyökét,
elsofoku a b = (-b)/a
-- - egy szám abszulút értékét,
abszolut a
    | a<0=-a
    |otherwise=a

abszolut2 a = if a<0 then -a else a
-- - egy szám előjelét,
elojel a = if a<0 then "Negativ" else "Pozitiv"

elojel2 a
    | n<0 = "Negativ"
    | n>0 = "Pozitiv"
    | otherwise = "Nulla"
-- - két argumentuma közül a maximumot,
maximum a b = if a<b then b else a
-- - két argumentuma közül a minimumot,
minimum a b = if a<b then a else b
-- - egy másodfokú egyenlet gyökeit,
-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
-- - az n szám faktoriálisát (3 módszer),
-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
-- - az első n négyzetszámot,
-- - az első n természetes szám köbét,
-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
-- - x hatványait adott n-ig,
-- - egy szám páros osztóinak listáját,
-- - n-ig a prímszámok listáját,
-- - n-ig az összetett számok listáját,
-- - n-ig a páratlan összetett számok listáját,
-- - az n-nél kisebb Pitágorászi számhármasokat,
-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
-- - azt a listát, ami felváltva tartalmaz True és False értékeket.

-- **Megoldott feladatok:**

-- - Határozzuk meg egy szám osztóinak listáját:

--   ```haskell
--   osztok :: Int -> [ Int ]
--   osztok n = [ i | i <- [1..n] , n `mod` i ==0]

--   > osztok 100
--   ```

-- - Határozzuk meg a következő listát: $$[(\texttt{a},0), (\texttt{b},1), \ldots, (\texttt{z}, 25)]$$:

--   ```haskell
--   import Data.Char
--   lista = [(chr(i + 97), i) | i<-[0..25]]

--   lista_ = zip ['a'..'z'] [1..26]
--   ```
