import Data.Array.Unboxed
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (ord)


letterNeighbours :: String -> [(Int, [Int])]
letterNeighbours stream = zip numbers $ (\xs -> zipWith (++) ([]:xs) (tail xs++[[]])) $ map (:[]) numbers
    where
        numbers = map letterValue stream

letterValue :: Char -> Int
letterValue x 
            | x <= '9' = ord x - 48 
            | otherwise = ord x - 87


subArray :: UArray (Int, Int) Int
subArray = array ((0,0),(35,35)) [((fst p, fst q), cost p q)| p <- letterGroups, q <- letterGroups] 
    where
        letterGroups :: [(Int,[Int])]
        letterGroups = concatMap letterNeighbours ["qwertyuiop","asdfghjkl","zxcvbnm","1234567890"]
        cost :: (Int, [Int]) -> (Int, [Int]) -> Int
        cost p q = 2 - (fromEnum $ (fst q) `elem` (snd p))

substitutionCost :: Char -> Char -> Int
substitutionCost a b = subArray ! (letterValue a, letterValue b)

-- letterNeighbours :: String -> [(Char, String)]
-- letterNeighbours stream = zip stream $ (\xs -> zipWith (++) ([]:xs) (tail xs++[[]])) $ map (:[]) stream
-- 
-- subArray :: UArray (Char, Char) Int
-- subArray = array (('a','a'),('z','z')) [((fst p, fst q), cost p q)| p <- letterGroups, q <- letterGroups] 
--     where
--         letterGroups :: [(Char,String)]
--         letterGroups = concatMap letterNeighbours ["qwertyuiop","asdfghjkl","zxcvbnm"]
--         cost :: (Char, String) -> (Char, String) -> Int
--         cost p q = 2 - (fromEnum $ (fst q) `elem` (snd p))
-- 
-- 
-- -- creates a map of the form q->w, w->qe etc. So every characters qwerty neighbours are known. Cost = 1 if the character is a keyboard
-- -- neighbour. cost = 2 otherwise    
-- substitutionCost :: Char -> Char -> Int
-- substitutionCost a b = subArray ! (a,b)
            
            
-- calculating the Leveishtein distance as described here http://en.wikipedia.org/wiki/Levenshtein_distance
-- Calculates it lazily backward taking advantage of the fact that I only want to know the distance if it is less than 2. Otherwise
-- I can approximate it to 3.\
distance :: UArray Int Char -> UArray Int Char -> Int
-- distance orig new = f m n
distance orig new = f m n
    where
        m = (snd.bounds) orig
        n = (snd.bounds) new
        memoArray = A.array ((0),(idx m n)) [((idx p q), f p q) | p <- [0..m], q<- [0..n]] 
        memf i j =  memoArray A.! (idx i j)
        idx i j = i*(n+1) + j
        f 0 i = i
        f j 0 = f 0 j
        f i j
            | abs (i - j) > 2 = 3
            | orig ! i == new ! j = memf (i-1) (j-1)
            | otherwise = minimum [memf (i-1) j + 1, memf i (j-1) + 1, memf (i-1)(j-1) + substitutionCost (orig ! i) (new ! j)]    

surcharge :: [UArray Int Char] -> BS.ByteString -> Int
surcharge domains bsCandidate
            | 0 `elem` distances = -1
            | null distances = 0
            | otherwise = 50 `div` minimum distances + truncate (logBase 10 (fromIntegral $ length distances))
        where 
            distances = filter (< 3) $ map (distance candidate) domains
            candidate = getArray bsCandidate        
            
surcharges :: ([UArray Int Char], [BS.ByteString]) -> [Int]            
surcharges (domains, candidates) = map (surcharge domains) candidates
        where
            domainMap = map (:[]) domains 
-- 
getArray :: BS.ByteString -> UArray Int Char
getArray xs = listArray (1, fromIntegral (BS.length xs)) (BS.unpack xs)

parse :: BS.ByteString -> ([UArray Int Char], [BS.ByteString])
parse input = (map getArray $ tail dlines, tail clines)
    where
        inlines = BS.lines input
        dCount = readInt.head.BS.words.head $ inlines
        (dlines, clines) = splitAt (dCount + 1) (tail inlines)

readInt :: BS.ByteString -> Int
readInt x =
  case BS.readInt x of Just (i,_) -> i
                       Nothing    -> error "Unparsable Int"        

main :: IO ()                    
main = do
    input <- BS.getContents
    let answers = surcharges $ parse input
    mapM_ print answers                    
                    
                      