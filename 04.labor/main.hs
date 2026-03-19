-- // # 4. labor

-- // I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- // - az első n páros szám négyzetét,
parosNegyzet n = take n [i**2 | i <- [2.0, 4.0 ..]]

parosNegyzet2 n = [i**2 | i <- [2.0, 4.0 .. (n*2)]]

-- // - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,

szamokLs n
    | n /= 1 = szamokLs (n-1) ++ replicate n n
    | otherwise = replicate n n

szamokLs2 n i
    | i /= n = replicate i i ++ szamokLs2 n (i+1)
    | otherwise = replicate i i

szamokLs3 n = [replicate i i | i <- [1, 2 .. n]]
-- // - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,

szamokLsN n
    | n /= 1 = szamokLsN (n-1) ++ replicate n (n*2)
    | otherwise = replicate n n

szamokLsN2 n i
    | i /= n = replicate i (i*2) ++ szamokLsN2 n (i+1)
    | otherwise = replicate i (i*2)

szamokLsN3 n = [replicate i (i*2) | i <- [1, 2 .. n]]

-- // - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$,
szamokLs4 n = [n, n-1 .. 1] ++ [1, 2 .. n]

szamokLs5 n = reverse  [1, 2 .. n] ++ [1, 2 .. n]
-- // - váltakozva tartalmazzon True és False értékeket,
valt n = take n ls
    where
        ls = [True, False] ++ ls --Vegtelen tomb!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

-- // - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.

valt2 n = take n ls
    where
        ls = [0, 1, -1] ++ ls
valt3 n i
    | mod i 3 == 0 = 0 : valt3 n (i+1)
    | mod i 3 == 1 = 1 : valt3 n (i+1)
    | otherwise = -1 : valt3 n (i+1)

-- // II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- // - meghatározza egy adott szám osztóinak számát,
osztok n = [i | i <- [1..n], mod n i == 0]

osztokdb n = length $ [i | i <- [1..n], mod n i == 0]

osztokdb2 n = myLength $ osztok n
    where
        myLength []=0
        myLength (_:ls) = 1 + myLength ls

osztokdb3 n = foldl (\res i -> if mod n i == 0 then res+1 else res) 0 [1..n]

-- // - meghatározza egy adott szám legnagyobb páratlan osztóját,
maxParatlan n = last $ filter odd $ osztok n

maxParatlan2 n
    | odd n=n
    | otherwise = last [i | i<- [1, 3 .. n], mod n i == 0]

maxParatlan3 n = foldl (\res i -> if mod n i == 0 then i else res) 1 [1, 3..n]

-- // - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
decPSzam x p = length $ decP x p

-- // - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
decPMax x p = maximum $ decP x p

decPMax2 x p = myMaximum $ decP x p
    where
        myMaximum [n] = n
        myMaximum (n1 : n2 : ls)
            | n1 > n2 = myMaximum (n1:ls)
            | otherwise = myMaximum (n2:ls)

-- // - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.
fibo = fiboSg 0 1 0
    where
        fiboSg a b res = res : fiboSg b res (res+b)

fiboAB a b = dropWhile (< a) $ takeWhile (< b) fibo

fiboAB2 a b = (dropWhile (< a) . takeWhile (< b)) fibo

-- // III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- // - meghatározza egy lista pozitív elemeinek átlagát,
atlag ls = sum ls / fromIntegral (length ls)

pozAtlag ls = atlag [i | i<-ls, i>0]

pozAtlag2 ls = (atlag . filter (>0)) ls
-- // - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
listaN ls n = [i | (idx, i) <- zip [1 ..] ls, mod idx n == 0]

listaN2 [x] n i = [x]
listaN2 ls n i
    | mod i n == 0 = ls !! i : listaN2 ls n (i+1)
    | otherwise = listaN2 ls n (i+1)

-- // - tükrözi egy lista elemeit,
-- // - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
-- // - meghatározza egy lista leggyakrabban előforduló elemét.
