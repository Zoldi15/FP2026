import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
-- # 1. labor

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: Int -> Int -> Int
osszeg a b = a+b

kivon :: Double -> Double -> Double
kivon a b = a-b

szorz :: Int -> Int -> Int
szorz a b = a*b

oszt :: Fractional a => a -> a -> a
oszt a b = a / b

oszt2 :: Integral a => a -> a -> a
oszt2 a b = a `div` b

oszt3 :: Integral a => a -> a -> a
oszt3 a b = a `div` b

osztmar :: Integral a => a -> a -> a
osztmar a b = mod a b
-- - egy első fokú egyenlet gyökét,
elsofoku a b = (-b)/a
-- - egy szám abszulút értékét,
abszolut a
    | a<0 =(-a)
    | otherwise =a

abszolut2 a = if a<0 then -a else a
-- - egy szám előjelét,
elojel a = if a<0 then "Negativ" else "Pozitiv"

elojel2 n
    | n<0 = "Negativ"
    | n>0 = "Pozitiv"
    | n==0 = "Nulla"
-- - két argumentuma közül a maximumot,
maximum a b = if a<b then b else a
-- - két argumentuma közül a minimumot,
minimum a b = if a<b then a else b
-- - egy másodfokú egyenlet gyökeit,
masodf a b c = if delta < 0 then error "Nincs valos gyok." else (gy1,gy2)
    where
        delta=b**2-4*a*c
        gy1=(-b+sqrt delta)/(2*a)
        gy2=(-b-sqrt delta)/(2*a)

masodf2 a b c
    | delta<0 = error "Nincs valos gyok."
    | delta == 0 = [gy1]
    where
        delta=b**2-4*a*c
        gy1=(-b+sqrt delta)/(2*a)
        gy2=(-b-sqrt delta)/(2*a)
-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
elempar ep1 ep2 = if(a==d && b==c) || (a==c && b==d) then True else False
    where
        (a,b)=ep1
        (c,d)=ep2

elempar2 (a,b) (c,d) = (a==d && b==c) || (a==c && b==d)
    
-- - az n szám faktoriálisát (3 módszer),
fakt1 0=1
fakt1 n=n*fakt1(n-1)

fakt2 n
    | n<0 = error "Negativ szam."
    | n==0 =1
    | otherwise = n*fakt2(n-1)

fakt3 n res
    | n<0 = error "Negativ szam."
    | n==0 =res
    | otherwise = n*fakt3(n-1) (res*n)

-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).
hatvany :: (Ord a, Floating a) => a -> a -> a
hatvany x n
    | n<0 = error "Negativ kitevo."
    | otherwise=x**n

hatvany2 :: (Num a, Integral b) => a -> b -> a
hatvany2 x n
    | n<0 = error "Negativ kitevo."
    | otherwise=x^n

hatvany3 x n
    | n<0 = error "Negativ kitevo."
    | n==0 = 1
    | otherwise=x*hatvany3 x (n-1)
-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
negyzetgy n = [sqrt i| i<-[1..n]]
-- - az első n négyzetszámot,
negyzet n = [i^2| i<-[0..n]]
-- - az első n természetes szám köbét,
kob n = [i^3| i<-[0..n]]
-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemnegyzet n = [i | i<-[0..n], i /= (sqrt i ** 2)]
-- - x hatványait adott n-ig,
xhatvany x n = [x^i | i<-[0..n]]
-- - egy szám páros osztóinak listáját,
parososzto n = [i | i<-[2..n/2], mod n i ==0, mod i 2 ==0]
-- - n-ig a prímszámok listáját,
osztok n = [i | i<-[1..n], mod n i ==0]
prim x = osztok x == [1,x]
primN n = [i | i<- [1..n], prim i]

primN2 n = [i | i<- [1..n], primL i]
    where
        primL si = osztokL si == [1,si]
        osztokL si2 = [i | i<-[1..si2], mod si2 i ==0]
-- - n-ig az összetett számok listáját,
osszetett n = [i | i<- [1..n], prim i == False] -- not(prim i)
-- - n-ig a páratlan összetett számok listáját,
osszetettparatlan n = [i | i<- [1..n], prim i == False, mod i 2 /=0] --odd i
-- - az n-nél kisebb Pitágorászi számhármasokat,
pitagorasz n = [(a,b,c) | c<- [1..n], b<- [1..n], a<- [1..n], a**2+b**2==c**2 || a**2+c**2==b**2 || c**2+b**2==a**2]
-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
betuszam = zip ['a'..'z'] [0..]
-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
lista1 = [(i,5-i) | i<-[0..5]]
-- - azt a listát, ami felváltva tartalmaz True és False értékeket.
tf1 n = [mod i 2 == 0 | i<-[0..n]]

tf2 n = take n ls
    where ls = [True,False] ++ ls

tf3 = [True,False] ++ tf3

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
--main = masodf 1 2 1

main :: IO ()
main = do
    putStrLn "Masodfoku egyenlet: "
    print(masodf 1 2 1)
    putStrLn ("Masodfoku egyenlet 2: " ++ show (masodf 4 5 6))
    putStrLn "Fakt: "
    print(fakt2 5)
    print(fakt2 7)
    putStrLn "Hatvany: "
    print(hatvany 2 5)
    putStrLn "Negyzet: "
    print(negyzet 8)
    putStrLn ("Kobnegyzet: " ++ show (kob 7))