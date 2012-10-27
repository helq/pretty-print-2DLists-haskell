import Data.Function (on)
import Data.List (sortBy, nub)
import PrintListBox (createBox', showListBox)

data Queen = Queen {positions :: [Int], size :: Int}

instance Show Queen where
  show q = show (queenToBox q)
  showList qss = showList $ map queenToBox qss

showLine :: Int -> Int -> String
showLine n q = init $ concat
      [if q==l
          then "# " -- "\ESC[95m#\ESC[0m " -- colorized 
          else "· " -- "\ESC[94m·\ESC[0m "
      | l<-[0..n-1] ]

queenToBox (Queen _  0) = createBox' [""] 0
queenToBox (Queen qs s) = createBox' (map (showLine s) qs) (2*s-1)

instance Eq Queen where
  (Queen ps s) == (Queen qs s') =
      s == s' &&
      any (ps ==) [qs,    reflect qs,    revQs,    reflect revQs
                  ,rotQs, reflect rotQs, revRotQs, reflect revRotQs ]
      -- any ((ps==).($ qs)) [id,     reflect,        reverse,        reflect.reverse
      --                     ,rotate, reflect.rotate, reverse.rotate, reflect.reverse.rotate]
        where reflect = map (lenQs - 1 -) -- reflect vertical
              rotate qs = map snd $ sortBy (compare `on` fst) $ zip qs [0..] -- rotation to right
              revQs = reverse qs -- reflect horizontal
              revRotQs = reverse rotQs
              lenQs = length qs
              rotQs = rotate qs

queens n = map (\q->Queen q n) $ placeQueens n
    where placeQueens 0 = [[]]
          placeQueens k = [ col:qs 
                          | qs <- placeQueens (k-1)
                          , col <- [0..n-1]
                          , isSafe' col qs ]
          
          isSafe' col qs = all safe $ zip qs [1..]
              where safe (q,n) = col /= q   &&
                                 col /= q+n &&
                                 col /= q-n

-- printQueens qs = putStr $ concatMap ((++"\n").show) qs

showQueensWith :: Int -> [Queen] -> [Char]
showQueensWith n = showListBox n . map queenToBox

main = do
    --print $ nub $ queens 8
    -- 95 is the size of the screen (terminal)
    putStr $ showQueensWith 95 $ nub $ queens 8
