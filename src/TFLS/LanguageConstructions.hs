module TFLS.LanguageConstructions
  ( subAlphabet
  , iteratedSymbol
  , startsWith
  , CFG(..)
  , contextFree
  , toCnf
  ) where

import TFLS.Basic
import Data.List

----------------------
-- Some general ways of constructing a language
----------------------

-- In practise it is really rare to define a language by plain enumeration of
-- its words. For one, only finite languages even have this option.
--
-- Some common patterns in language definition are presented in this section.

-- A free monoid over a subset of Char (represented by a string). Filters out
-- strings containing wrong characters.
subAlphabet :: String -> Language
subAlphabet alphabet st = filter (`elem` alphabet) st == st

-- Wffs consist only of a particular character
iteratedSymbol :: Char -> Language
iteratedSymbol a = subAlphabet [a]

-- Wffs are any string starting with a particular character
startsWith :: Char -> Language
startsWith a (b:bs) = a == b
startsWith _ []     = False

-- Wffs that contain a particular character
contains :: Char -> Language
contains c st = c `elem` st

-- Context-free languages

type Rule      = (Char,String)
type StartSymb = Char
data CFG       = Grammar [Rule] StartSymb
  deriving (Show)

-- CYK algorithm
contextFree :: CFG -> Language
contextFree g@(Grammar _ s) st =
  (s `elem`) $ last $ head $ last $
  take (length st) $ iterate (cykStep g') (cykInit g' st)
    where g' = toCnf g

cykInit :: CFG -> String -> [[String]]
cykInit (Grammar rs s) =
  map $ \c -> [map fst $ filter (\(v,t) -> head t == c) rs]

cykStep :: CFG -> [[String]] -> [[String]]
cykStep g s = zipRows (newRow g s) s

newRow :: CFG -> [[String]] -> [String]
newRow g (w:ws) = (mconcat $ map (undoRules g) $
                  processColumn w ws):(newRow g ws)
newRow _ []     = []

processColumn :: [String] -> [[String]] -> [String]
processColumn (x:xs) (w:ws) = (prod x $ (w !! (length xs)))++(processColumn xs ws)
processColumn []     _      = []
processColumn _      []     = []

prod :: [a] -> [a] -> [[a]]
prod as bs = map (\(a,b) -> [a,b]) [(a,b)|a<-as,b<-bs]

undoRules :: CFG -> String -> String
undoRules (Grammar rs s) t = map fst $ filter (mapsTo t) rs
  where mapsTo t r = snd r==t

zipRows :: [String] -> [[String]] -> [[String]]
zipRows (x:xs) (w:ws) = (w++[x]):(zipRows xs ws)
zipRows _      []     = []

-- get the vocabulary of a context-free language
getVocab :: CFG -> String
getVocab (Grammar r s) = (nub . mconcat . (map snd)) r

-- get variables of a context-free language
getVars :: CFG -> String
getVars (Grammar r s) = (nub . (map fst)) r

-- get the alphabet of a context-free language
getAlph :: CFG -> String
getAlph g = (getVocab g) \\ (getVars g)

-- convert a CFG to Chomsky normal form

toCnf :: CFG -> CFG
toCnf = toCnfUnit . toCnfBin . toCnfTerm . toCnfStart
-- TODO add a toCnfDel rule

toCnfStart, toCnfTerm, toCnfBin, toCnfUnit :: CFG -> CFG

toCnfStart g@(Grammar rs s) = Grammar ((s',[s]):rs) s'
  where s' = head $ newChars 1 $ getVocab $ g
        
toCnfTerm g@(Grammar rs s) = Grammar (rs'++(subsRules rs' rs)) s
  where rs' = genRules (getNonsolTerms g) g

getNonsolTerms :: CFG -> String
getNonsolTerms g@(Grammar r s) =
  mconcat $
  map ((intersect (getAlph g)) . snd) $
  filter isNonsol r
    where isNonsol a = length (snd a) > 1

genRules :: String -> CFG -> [Rule]
genRules st g = zip (newChars (length st) (getVars g)) (map (\a -> [a]) st)

subsRules :: [Rule] -> [Rule] -> [Rule]
subsRules rs1 rs2 = map (subsRule rs1) rs2

subsRule :: [Rule] -> Rule -> Rule
subsRule rs (v,st) = (v,map (subsSymbol rs) st)

subsSymbol :: [Rule] -> Char -> Char
subsSymbol ((v,t):rs) c
  | c == head t = v
  | otherwise   = subsSymbol rs c
subsSymbol [] c = c

toCnfBin g@(Grammar rs s) = Grammar (mconcat $ map (ruleBin newvars) rs) s
  where newvars = newChars 24 (getVars g)

ruleBin :: String -> Rule -> [Rule]
ruleBin (b1:bs) (v,[a1,a2,a3])       = [(v,[a1,b1]),(b1,[a2,a3])]
ruleBin (b1:b2:bs) (v,(a1:a2:a3:as)) =
  (v,[a1,b1]):(b1,[a2,b2]):(ruleBin bs (b2,a3:as))
ruleBin _ (v,a)                      = [(v,a)]

toCnfUnit g@(Grammar rs s)
  | length (filter (isUnit g) rs) == 1 =
      Grammar ((unitReplacement g rs)++(filter (not . (isUnit g)) rs)) s
  | length (filter (isUnit g) rs) >  1 =
      toCnfUnit (Grammar ((unitReplacement g rs)++(filter (not . (isUnit g)) rs)) s)
  | otherwise                          = g

isUnit :: CFG -> Rule -> Bool
isUnit g (v,[c]) = c `elem` (getVars g)
isUnit _ _       = False

unitReplacement :: CFG -> [Rule] -> [Rule]
unitReplacement g rs = mconcat $ map (filtered rs) rs'
  where rs' = filter (isUnit g) rs

filtered :: [Rule] -> Rule -> [Rule]
filtered rs (v,st) =
  map (\(v',st') -> (v,st')) $ filter (\(v',st') -> v' == head st) rs

-- A helper function that gets characters not already in the alphabet
newChars :: Int -> String -> String
newChars n s = take n $ (['A'..'Z'] \\ s)

