import System.Win32 (LOCALESIGNATURE(lsCsbDefault), SECURITY_ATTRIBUTES (nLength))

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szamszorz 0=1
szamszorz n = (mod n 10)* szamszorz (div n 10)

szamszorz2 n
    | n<0 = szamszorz2(abs n)
    | div n 10 == 0 = mod n 10 
    | otherwise = mod n 10 * szamszorz2(div n 10)

ls1 = [324, 56, 98, 72]
szjSzorz ls = map szamszorz2 ls
-- - egy szám számjegyeinek összegét (2 módszerrel),
szamossz n = (mod n 10)+ szamossz (div n 10)

szamossz2 n
    | n<0 = szamossz2(abs n)
    | div n 10 == 0 = mod n 10 
    | otherwise = mod n 10 + szamossz2(div n 10)

szjOssz ls = map szamossz2 ls

szjOssz2 ls = map (\x -> (x, szamossz2 x 0)) ls
-- - egy szám számjegyeinek számát (2 módszerrel),

szamjegy 0=0
szamjegy n = szamjegy(div n 10)+1

szamjegy2 n
    | n<0 = szamjegy2 (abs n)
    | div n 10 == 0 = 1
    | otherwise = szamjegy2(div n 10)+1

szamjegy3 n res
    | n<0 = szamjegy3 (abs n) res
    | n<10 = res+1
    | otherwise = szamjegy3(div n 10)(res+mod n 10)

szjDb ls = map szamjegy ls

-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:
fugv4 n szj
    | szj>=10 = error "Nem szamjegy"
    | div n 10==0 = if mod n 10 == szj then szj+div n 10 else szj
    | otherwise = if mod n 10 == szj then szj + fugv4(div n 10) szj else fugv4(div n 10) szj

fugv44 n szj res
    | szj>=10 = error "Nem szamjegy"
    | n<10 = if n == szj then res*res else res
    | otherwise = if mod n 10 == szj then fugv44(div n 10) szj (res+1) else fugv44(div n 10) szj res

ls2=[(577578, 7),(0,1),(2847, 2)]

szjOssz3 ls = map (uncurry fugv4) ls

--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```
-- - egy szám páros számjegyeinek számát,
parosdb n = if mod n 2==0 then parosdb (div n 10) + 1 else parosdb (div n 10)

parosdb2 :: (Integral a, Num t) => a -> t
parosdb2 n
    | n<0 = parosdb2 (abs n)
    | n<10 = if even n then 1 else 0
    | otherwise = if even (mod n 10) then 1 + parosdb2(div n 10) else parosdb2(div n 10)

parosSzj = map parosdb2 ls1
-- - egy szám legnagyobb számjegyét,
legnagy n lg
    | n<0 = legnagy(abs n) lg
    | n<10 = max lg n
    | otherwise = if mod n 10>lg then legnagy(div n 10) (mod n 10) else legnagy(div n 10) lg

lgSzj = map (\x -> legnagy x (-1)) ls1
-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:

--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2

bSzamdSzj n b d
    | n<0 = error "Negativ szam."
    | n<b = if n == d then 1 else 0
    | otherwise = if mod n b == d then 1+bSzamdSzj(div n b) b d else bSzamdSzj(div n b) b d
--   ```
ls3 = [(577578, 10, 7),(1024,2, 1),(2847, 2, 3)]
bSzamSzjLs = map (\(n, b, d) -> bSzamdSzj n b d) ls3
-- - az 1000-ik Fibonacci számot.
fibo 0=1
fibo 1=1
fibo n = fibo (n-1)+fibo(n-2)

fiboN n = fibon 0 1 0 n
    where 
        fibon _ _ res 0 = res
        fibon a b res n1 = fibon b res (b+res)(n1-1) 

fiboN2 n = fibon2 1 0 n
    where
        fibon2 res _ 0 = res
        fibon2 res b n1 = fibon2 (b + res) res (n1 - 1)


-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

-- **Megoldott feladatok:**

-- - Határozzuk meg egy szám számjegyeinek összegét:
--   I. módszer:

--   ```haskell
--   szOsszeg :: Int -> Int
--   szOsszeg 0 = 0
--   szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

--   > szOsszeg 123
--   ```

--   II. módszer:

--   ```haskell
--   szOsszeg1 :: Int -> Int -> Int
--   szOsszeg1 0 t = t
--   szOsszeg1 x t = szOsszeg1 (x `div` 10) ( t + x `mod` 10 )

--   > szOsszeg1 123 0
--   ```
