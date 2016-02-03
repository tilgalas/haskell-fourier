module Main where

import Svg
import Data.Complex
import Dft
import Data.Foldable
import System.Environment
import Data.Maybe

_N = 256 :: Int
_N_Minus1 = _N - 1 :: Int

square :: (Enum a, Floating a) => Int -> [a]
square n = [sum [sin ((fromIntegral (2 * (k * 2 + 1) * x)) * pi / (fromIntegral _N)) / (fromIntegral (k * 2 + 1)) | k <- [0..n-1]] | x <- [0.._N_Minus1]]

sawtooth = 0 : [1 - (2 / (fromIntegral _N)) * (fromIntegral x) | x <- [1.._N_Minus1]]

approxSawtooth :: (Enum a, Floating a) => Int -> [a]
approxSawtooth n = [foldr1 (+) [sin (fromIntegral (2 * k * x) * pi / (fromIntegral _N)) / (fromIntegral k) | k <- [1..n]] | x <- [0.._N_Minus1]]

signal = approxSawtooth 10

realToComplex :: (Floating a) => a -> Complex a
realToComplex a = a :+ 0

main :: IO ()
main = do
  args <- getArgs
  let f = (read . head) args
      graphStroke = Stroke (Just "black") (Just 0.03)
      graphScale = 8 / (fromIntegral _N)
      graphTransformPair = TransformPair ("scale(" ++ show graphScale ++ " 1)")
                           ("scale(" ++ show (1 / graphScale) ++ " 1)")
      dasharray = fromMaybe "2" ((show . (*2)) <$> strokeWidthMaybe graphStroke) ++
                  fromMaybe " 1" (((' ' :) . show) <$> strokeWidthMaybe graphStroke)
      helperStroke = strokeWidthMap (/4) graphStroke
  for_ (zip (animateFreq (map realToComplex signal) f) [0..])
    (\(content, num) ->
      writeFile ("graph" ++ show num ++ ".svg") (
        prologue Nothing "xlink" (Just (WidthHeight "600" "600")) (Just $ ViewBox "-5" "-5" "10" "10") ++
        content ++
        "<g transform=\"translate(-4 -3)\">" ++
        foldr1 (++) (map (\i -> let x = fromIntegral (i * _N) / (fromIntegral f) in
                           "<" ++ maybePrefix Nothing ++
                           "line" ++ strokeToString helperStroke ++
                           " stroke-dasharray=\"" ++
                           dasharray ++
                           "\" x1=\"0\" y1=\"1\" x2=\"0\" y2=\"-1\"" ++
                           " transform=\"translate(" ++ show (x * graphScale) ++ " 0)\" />") [0..f]) ++
        "<" ++ maybePrefix Nothing ++ "line" ++ strokeToString helperStroke ++ 
        " stroke-dasharray=\"" ++ dasharray ++ "\" x1=\"" ++ show (-5 * graphScale) ++ "\" y1=\"0\"" ++
        " x2=\"" ++ show ((fromIntegral (_N + 5)) * graphScale) ++ "\" y2=\"0\" " ++
        " />" ++
        drawGraph (zip [0..(fromIntegral _N_Minus1)] signal) (Just num) Nothing (Just graphTransformPair) graphStroke ++
        "</g>" ++
        epilogue Nothing))
  
    
  
animateFreq :: (RealFloat a, Show a) => [Complex a] -> Int -> [String]
animateFreq signal f = 
  map partToString ((tail . reverse) (correlateSignalAtFreq signal f)) where
    partToString (s, wavePart, sum) =
      drawComplex (sum / (2 * sqrt (fromIntegral (length signal)) :+ 0.0)) Nothing (Stroke (Just "blue") (Just 0.03)) ++      
      drawComplex wavePart Nothing (Stroke (Just "green") (Just 0.02)) ++
      drawComplex s Nothing (Stroke (Just "red") (Just 0.01))

