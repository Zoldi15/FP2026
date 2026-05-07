-- # 7. labor

-- I. Írjunk egy-egy Haskell függvényt, amely

-- - az n-nél kisebb négyzetszámokat kiírja egy szövegállományba,
-- - az n-nél kisebb számok négyzetgyökét kiírja egy szövegállományba. A nr négyzetgyök meghatározásához használjuk a következőket, ahol az iterációt addig kell végezni, amíg $x_{n+1}$ nem egyenlő $x_n$-nel:

--   ```
--   x_0 = 1
--   x_{n+1} = (x_n + nr/x_n)/2
--   ```
-- - az n-nél kisebb számok köbgyökét kiírja egy szövegállományba.A köbgyök meghatározásához használjuk a következőket, ahol az iterációt addig kell végezni, amíg $x_{n+1}$ nem egyenlő $x_n$-nel:

--   ```
--   x_0 = 1
--   x_{n+1} = (2·x_n + nr/(x_n·x_n))/3



writeSquares n file =
    writeFile file $
        unlines [show (x*x) | x <- [0..], x*x < n]

sqrtNewton nr = i 1
    where
    i x =
        let x2 = (x + nr / x) / 2
        in if x == x2 then x else i x2

writeSquareRoots n file =
    writeFile file $
        unlines [show i ++ " -> " ++ show (sqrtNewton (fromIntegral i))
                | i <- [1..n-1]]

cbrtNewton nr = i 1
    where
    i x =
        let x2 = (2*x + nr/(x*x)) / 3
        in if x == x2 then x else i x2

writeCubeRoots n file =
    writeFile file $
        unlines [show i ++ " -> " ++ show (cbrtNewton (fromIntegral i))
                | i <- [1..n-1]]

-- II. Írjunk egy-egy Haskell függvényt, amely szövegállományban levő számokat olvas be egy listába, és kiírja formázva egy másik szövegállományba

-- - a számok rendezett sorrendjét,
-- - a számokkal együtt a számok 2-es számrendszerbeli alakját, illetve, hogy hány egyes szerepel a 2-es számrendszerbeli alakban,
-- - a számokkal együtt a számok 2, 16, 256-os számrendszerbeli alakját,
-- - a számokkal együtt a számok prímosztóit.

readNumbers :: FilePath -> IO [Int]
readNumbers file = do
    content <- readFile file
    return (map read (words content))

writeSorted :: FilePath -> FilePath -> IO ()
writeSorted input output = do
    nums <- readNumbers input
    writeFile output $
        unlines (map show (sort nums))

toBinary 0 = "0"
toBinary n = reverse (helper n)
    where
    helper 0 = ""
    helper x =
        let (q,r) = divMod x 2
        in intToDigit r : helper q

countOnes = length . filter (=='1')

writeBinaryInfo :: FilePath -> FilePath -> IO ()
writeBinaryInfo input output = do
    nums <- readNumbers input
    writeFile output $
        unlines [show n ++" bin=" ++ b ++" ones=" ++ show (countOnes b) | n <- nums, let b = toBinary n]

toBase b n = showIntAtBase b intToDigit n ""

writeBaseInfo :: FilePath -> FilePath -> IO ()
writeBaseInfo input output = do
    nums <- readNumbers input
    writeFile output $
        unlines
        [show n ++" bin=" ++ toBase 2 n ++" hex=" ++ toBase 16 n ++ " base256=" ++ toBase 256 n | n <- nums]

primeFactors n = factor n 2
    where
    factor 1 _ = []
    factor x d
        | x `mod` d == 0 = d : factor (x `div` d) d
        | otherwise      = factor x (d+1)

writePrimeFactors :: FilePath -> FilePath -> IO ()
writePrimeFactors input output = do
    nums <- readNumbers input
    writeFile output $
        unlines
        [show n ++ " -> " ++ show (primeFactors n) | n <- nums]

-- III. Írjunk egy Haskell függvényt, amely amely kigenerálja egy állományba

-hamming :: [Int]
hamming =
    1
    : merge3
      (map (2 *) hamming)
      (map (3 *) hamming)
      (map (5 *) hamming)

merge :: [Int] -> [Int] -> [Int]
merge (x : xs) (y : ys)
    | x < y = x : merge xs (y : ys)
    | x > y = y : merge (x : xs) ys
    | otherwise = x : merge xs ys

merge3 a b c = merge a (merge b c)

isHamming n = maybe False (all (`elem` [2, 3, 5])) (primosztok n)

hammingAB a b =
    if a < b
    then dropWhile (< a) $ takeWhile (< b) hamming
    else dropWhile (< b) $ takeWhile (< a) hamming

hammingAB2 a b = (dropWhile (< a) . takeWhile (< b)) [i | i <- [2 ..], isHamming i]

hammingAB3 a b = [i | i <- [a .. b], isHamming i]

mainHamming = do
    putStr "a="
    a <- readLn :: IO Int
    putStr "b="
    b <- readLn :: IO Int
    let lsH = hammingAB a b
        lsH2 = if a < b then hammingAB a b else hammingAB b a
    --   writeFile "07.labor/hammingSzamokAB.txt" $ unlines (map show lsH) --kulon sorba
    writeFile "07.labor/hammingSzamokAB.txt" $ unwords (map show lsH) -- egy sorba

-- - az 10000-nél kisebb prímszámokat, a prímszámokat Eratoszthenész szitájával határozzuk meg,
primek = szita [2 ..]
    where
    szita (p : xs) = p : szita [x | x <- xs, x `mod` p /= 0]

primekN n = takeWhile (< n) primek

mainPrimek = do
    let n = 10000
        ls = primekN n
    writeFile "07.labor/primekN.txt" $ unwords (map show ls)

-- - az 10000-nél kisebb szerencsés számokat ([Lucky number](https://en.wikipedia.org/wiki/Lucky_number)).
mySelect :: (Integral a1) => [a2] -> a1 -> [a2]
mySelect ls n = [snd x | x <- filter (fgFilter n) $ zip [1 ..] ls]
    where
    fgFilter n (i, nr) = mod i n /= 0

luckyNr :: (Integral a) => Int -> [a]
luckyNr n = 1 : (take n $ rek 2 [1, 3 ..])

rek :: (Integral a) => Int -> [a] -> [a]
rek i tls = e : rek (i + 1) ls
    where
    ls = mySelect tls e
    e = tls !! (i - 1)

szerencses = 1 : szerencses' [1, 3 ..] 2
    where
    szerencses' xs n =
        let p = xs !! (n - 1)
            xs' = elhagySzam p xs
        in p : szerencses' xs' (n + 1)
    elhagySzam k ys = [y | (y, i) <- zip ys [1 ..], i `mod` k /= 0]

szerencses10000 = takeWhile (< 10000) szerencses

fajlbaIII = do
    putStr "a="
    a <- readLn :: IO Int
    putStr "b="
    b <- readLn :: IO Int
    let h = hammingAB a b
        n = 10000
        p = primekN n
        l = szerencses10000
        file = "07.labor/romai3.txt"

    writeFile file $
    "Hamming szamok ("
        ++ show a
        ++ ", "
        ++ show b
        ++ "):\n"
        ++ show h
        ++ "\n\n"
        ++ "Primszamok 10000 alatt:\n"
        ++ show p
        ++ "\n\n"
        ++ "Szerencses szamok 10000 alatt:\n"
        ++ show l

-- IV. Írjunk egy-egy Haskell függvényt, amely

-- - meghatározza, hogy két bináris állományban milyen pozíciókon található különböző bájt,
binKulonbozoPos :: FilePath -> FilePath -> IO [Int]
binKulonbozoPos fajl1 fajl2 = do
    b1 <- BS.readFile fajl1
    b2 <- BS.readFile fajl2
    return (kulonbozoek (BS.unpack b1) (BS.unpack b2) 0)
    where
        kulonbozoek [] [] _ = []
        kulonbozoek (x : xs) (y : ys) i
            | x /= y = i : maradek
            | otherwise = maradek
            where
            maradek = kulonbozoek xs ys (i + 1)

    -- kulonbozo hossz
    kulonbozoek [] ys i = [i .. i + length ys - 1]
    kulonbozoek xs [] i = [i .. i + length xs - 1]

-- - megvizsgálja, hogy egy adott bájtszekvencia benne van-e egy bináris állományban,
tartBajtszekvencia fajl bsz = do
    let bszFormaz = BC.pack bsz -- amennyiben stringet adunk meg at kell alakitani bajtszekvenciava, amennyiben mar bsz ez a lepes kihagyhato
    content <- BS.readFile fajl
    return (bszFormaz `BS.isInfixOf` content)

-- - meghatározza egy adott állomány bájtméretét, ahol az állománynevet a billentyűzetről olvassuk be,
fajlmeret :: IO ()
fajlmeret = do
    putStrLn "fajl neve:"
    path <- getLine
    handle <- openFile path ReadMode
    size <- hFileSize handle
    hClose handle
    putStrLn ("fajlmeret: " ++ show size ++ " bajt")

-- - meghatározza bináris állományok méret szerinti rendezett sorrendjét, ahol az állományneveket a billentyűzetről olvassuk be,
rendezettBinFajlok = do
    let fajlok = ["07.labor/noveny.jpg", "07.labor/oriaskerek.jpg"]
    pairs <- forM fajlok $ \f -> do
    handle <- openBinaryFile f ReadMode
    size <- hFileSize handle
    hClose handle
    return (f, size)

    let sorted = sortOn snd pairs

    putStrLn "\nFajlok meret szerint rendezve:"
    mapM_ printPair sorted

readFiles :: IO [FilePath]
readFiles = do
    line <- getLine
    if null line
        then return []
        else do
            rest <- readFiles
            return (line : rest)

printPair :: (FilePath, Integer) -> IO ()
printPair (f, s) = putStrLn (f ++ " -> " ++ show s ++ " bytes")

rendezettBinFajlok2 = do
    putStrLn "Add meg a fajlneveket (ures sor = vege):"
    files <- readFiles

    pairs <- forM files $ \f -> do
        -- forM_ is just like for_, but specialised to monadic actions. for_ is just like forM_, but generalised to Applicative actions.
        size <- getFileSize f
        return (f, size)

    let sorted = sortOn snd pairs

    putStrLn "\nFajlok meret szerint rendezve:"
    mapM_ printPair sorted

-- - másolatot készít bináris állományokról, ahol az állományok nevét a billentyűzetről olvassuk be,
masolFajlok = do
    putStrLn "Add meg a fajlneveket (ures sor = vege):"
    --   files <- readFiles
    let files = ["noveny.jpg"]
    eredmenyek <- forM files $ \src -> do
        content <- BS.readFile src
        let dst = takeWhile (/= '.') src ++ "_copy" ++ dropWhile (/= '.') src
        -- let dst = "07.labor/noveny_cp.jpg"
        BS.writeFile dst content
        return True
    if and eredmenyek
    then putStrLn "Masolas kesz!"
    else putStrLn "Nem sikerult!"

-- V. Írjunk egy Haskell programot, amely titkosítja karakterek (bájtok) egy adott listáját, majd vissza is fejti a rejtjelezett értéket:

-- - a titkosításhoz egy titkos információt, egy kulcsot (karaktereket/bájtokat) kell megadni,
-- - a titkosítás azt fogja jelenti, hogy a bemeneti bájtok és a kulcs bájtjai között alkalmazzuk az xor műveletet, úgy hogy a kulcs bájtjait körkörösen vesszük, ami azt jelenti, hogy ha elfogytak a kulcs bájtjai, akkor a kulcs első bájtjával folytatjuk az xor műveletet, egészen addig, amíg a bemenet bájtjain is végig nem mentünk,
-- - a helyes működés miatt fontos, hogy ugyanazt a kulcsot használjuk mind a titkosításhoz, mind a visszafejtéshez,
-- - a titkosított értéket hexadecimális string-ként írjuk ki,
-- - a program során legyen választási lehetőség arra vonatkozóan, hogy a kulcs értékét:
--   - beolvassuk a billentyűzetről, mint hexadecimális string
--   - véletlenszerűen generáljuk, mint 0 és 255 közötti természetes számok.

-- Például:

-- ```haskell
-- > bemenet = "sapientia marosvasarhelyi tudomanyegyetem"
-- > kulcs = "c 38 ff 66 71 22 38 4e 79 65"
-- > cryptStr bemenet kulcs
-- titkositott ertek: 7f 59 8f f 14 4c 4c 27 18 45 61 59 8d 9 2 54 59 3d 18 17 64 5d 93 1f 18 2 4c 3b 1d a 61 59 91 1f 14 45 41 2b d 0 61
-- ```
-- string <-> bajt konverzio
strToBytes :: String -> [Word8]
strToBytes str = map (fromIntegral . fromEnum) str

byteToStr :: [Word8] -> String
byteToStr byte = map (toEnum . fromIntegral) byte

-- hexa atalakitas
toHex :: [Word8] -> String
toHex byte =
    unwords $
    map
        ( \b ->
            let h = showHex b ""
            in if length h == 1
                then '0' : h
                else h
        )
    byte

fromHex :: String -> [Word8]
fromHex str = map (fst . head . readHex) (words str)

-- xor ismetlodo kulccsal
xorWithKey :: [Word8] -> [Word8] -> [Word8]
xorWithKey input key = zipWith xor input (cycle key)

-- titkositas
cryptStr :: String -> [Word8] -> String
cryptStr input key = toHex $ xorWithKey (strToBytes input) key

-- visszafejtes
decryptStr :: String -> [Word8] -> String
decryptStr hexInput key = byteToStr $ xorWithKey (fromHex hexInput) key

-- random generalas
randomKey :: Int -> IO [Word8]
randomKey n = replicateM n (randomRIO (0, 255))

main :: IO ()
main = do
    putStrLn "Bemeneti szoveg:"
    input <- getLine

    putStrLn "Kulcs mod:"
    putStrLn "1 - Hex string"
    putStrLn "2 - Veletlen kulcs"
    mode <- getLine

    key <- case mode of
        "1" -> do
            putStrLn "Add meg a kulcsot (hex, pl: c 38 ff 66):"
            k <- getLine
            return (fromHex k)
        "2" -> do
            putStrLn "Kulcs hossza:"
            len <- readLn
            randomKey len
        _ -> error "Ervenytelen valasztas"

    let encrypted = cryptStr input key

    putStrLn "\nTitkositott (hex):"
    putStrLn encrypted

    let decrypted = decryptStr encrypted key

    putStrLn "\nVisszafejtett:"
    putStrLn decrypted