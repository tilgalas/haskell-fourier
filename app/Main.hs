module Main where

import Svg
import Data.Complex
import Dft
import Data.Foldable
import System.Environment

square :: (Enum a, Floating a) => Int -> [a]
square n = [foldr1 (+) [sin ((fromIntegral (2 * (k * 2 + 1))) * pi * x / 256) / (fromIntegral (k * 2 + 1)) | k <- [0..n-1]] | x <- [0..255]]

signal = square 10

realToComplex :: (Floating a) => a -> Complex a
realToComplex a = a :+ 0

main :: IO ()
main = do
  args <- getArgs
  let _N = (read . head) args
      graphStroke = Stroke (Just "black") (Just 0.03)
  for_ (zip (animateFreq (map realToComplex signal) _N) [0..])
    (\(content, num) ->
      writeFile ("graph" ++ show num ++ ".svg") (
        prologue Nothing "xlink" (Just (WidthHeight "600" "600")) (Just $ ViewBox "-5" "-5" "10" "10") ++
        content ++
        "<g transform=\"translate(-4 -3) scale(" ++ show (8.0 / 256.0) ++ " 1)\">" ++
        drawGraph (zip [0..255] signal) (Just num) Nothing graphStroke ++
        foldr1 (++) (map (\i -> let x = show (fromIntegral (i * 256) / (fromIntegral _N)) in
                           "<" ++ maybePrefix Nothing ++
                           "line " ++ strokeToString graphStroke ++
                           " x1=\"0\" y1=\"1\" x2=\"0\" y2=\"-1\"" ++
                           " transform=\"translate(" ++ x ++ " 0) scale(" ++ show (256.0 / 8.0) ++ " 1)\" />") [0.._N]) ++
        "</g>" ++
        epilogue Nothing))
  
    
  
animateFreq :: (RealFloat a, Show a) => [Complex a] -> Int -> [String]
animateFreq signal f = 
  map partToString ((tail . reverse) (correlateSignalAtFreq signal f)) where
    partToString (s, wavePart, sum) =
      drawComplex (sum / (2 * sqrt (fromIntegral (length signal)) :+ 0.0)) Nothing (Stroke (Just "blue") (Just 0.03)) ++      
      drawComplex wavePart Nothing (Stroke (Just "green") (Just 0.02)) ++
      drawComplex s Nothing (Stroke (Just "red") (Just 0.01))

