module Util where

lastNWindow :: Int -> [a] -> [[a]]
lastNWindow 0 l = map (const []) l 
lastNWindow n l = tail $ scanl (\oldWindow newElem -> newElem : (take (n - 1) oldWindow)) [] l
