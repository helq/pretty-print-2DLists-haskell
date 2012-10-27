module PrintListBox (
      Box(..)
    , createBox
    , (+|+)
    ) where

data Box = Box {box :: [String], width :: Int}

maxSize = 80

instance Show Box where
    --show (Box xs w) = "Box " ++ show xs ++ " " ++ show w
    show (Box xs w) = concatMap (++"\n") xs
    
    showList [] _ = "[]"
    showList bs _ = (('\n':)) $ concatMap ((++"\n").show)
                  $ concatBoxSWithMax $ [leftSquare]
                    ++ map (+|+ comma) (init bs) ++ [last bs, rightSquare]
        where 
          concatBoxSWithMax (b:b':bs)
            | width b + width b' > maxSize = b : concatBoxSWithMax (b':bs)
            | otherwise                    = concatBoxSWithMax (b+|+b':bs)
          concatBoxSWithMax bs = bs

          height = length $ box $ head bs
          width' = width $ head bs

          createBracket u m d s
             | height <= 1 = [s]
             | otherwise   = [u] ++ [m|_<-[1..height-2]] ++ [d]

          leftSquare  = Box (createBracket "⎡" "⎢" "⎣" "[") 1
          rightSquare = Box (createBracket "⎤" "⎥" "⎦" "]") 1

          comma = Box (createBracket "   " "   " ",  " ",  ") 3

createBox :: [String] -> Box
createBox xs = Box xs' width
    where width = minimum $ map length xs
          xs' = map (take width) xs

(+|+) :: Box -> Box -> Box
(Box bs w1) +|+ (Box bs' w2) = Box concatBss (w1+w2)
        where --concatBss = zipWith (++) bs bs'
              concatBss = unsafeZipAdd bs bs'
              unsafeZipAdd []     []     = []
              unsafeZipAdd (x:xs) (y:ys) = (x++y) : unsafeZipAdd xs ys
              unsafeZipAdd _      _      = error "Only Boxes of same size"
