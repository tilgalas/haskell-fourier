module Main where

import Svg
import Data.Complex
import Dft
import Data.Foldable
import System.Environment
import Data.Maybe
import Util
import qualified Data.Vector as V
import Numeric.FFT.Vector.Invertible

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

freqDomain :: [Complex Double]
freqDomain = let timeDomain = (V.fromList signal) in
  V.toList $ run dftR2C timeDomain

freqDomainAmp :: [Double]
freqDomainAmp = let (h : t) = freqDomain
                    (m, l) = splitAt (length t - 1) t in
  (magnitude h / 2) : (map magnitude m) ++ (map ((/2) . magnitude) l)


prefix :: Maybe String
prefix = Nothing

main :: IO ()
main = do
  args <- getArgs
  let f = (read . head) args
      graphStroke = Stroke (Just "black") (Just 0.03)
      graphScale = 8 / (fromIntegral _N)
      dasharray = fromMaybe "2" ((show . (*2)) <$> strokeWidthMaybe graphStroke) ++
                  fromMaybe " 1" (((' ' :) . show) <$> strokeWidthMaybe graphStroke)
      helperStroke = strokeWidthMap (/4) graphStroke
      signalGraphTransform = "translate(-4 3)"
  writeFile "background.svg" (
    prologue prefix "xlink" (Just (WidthHeight "600" "600")) (Just $ ViewBox "-5" "-5" "10" "10") ++
        "<" ++ maybePrefix prefix ++ "g transform=\"" ++ signalGraphTransform ++ "\">" ++
        foldr1 (++) (map (\i -> let x = fromIntegral (i * _N) / (fromIntegral f) in
                           "<" ++ maybePrefix prefix ++
                           "line" ++ strokeToString helperStroke ++
                           " stroke-dasharray=\"" ++
                           dasharray ++
                           "\" x1=\"0\" y1=\"1\" x2=\"0\" y2=\"-1\"" ++
                           " transform=\"translate(" ++ show (x * graphScale) ++ " 0)\" />") [0..f]) ++
        "<" ++ maybePrefix prefix ++ "line" ++ strokeToString helperStroke ++ 
        " stroke-dasharray=\"" ++ dasharray ++ "\" x1=\"" ++ show (-5 * graphScale) ++ "\" y1=\"0\"" ++
        " x2=\"" ++ show ((fromIntegral (_N + 5)) * graphScale) ++ "\" y2=\"0\" " ++
        " />" ++
        drawGraph prefix graphScale 1.0 graphStroke (zip [0..(fromIntegral _N_Minus1)] signal) ++
        "</" ++ maybePrefix prefix ++ "g>" ++
        "<" ++ maybePrefix prefix ++ "g transform=\"translate(-4 -4)\">" ++
        drawGraph prefix (graphScale * 2) freqDomainScale graphStroke (zip [0..(fromIntegral _N_Minus1)] freqDomainAmp) ++
        "<" ++ maybePrefix prefix ++ "circle cx=\"" ++ show ((fromIntegral f) * graphScale * 2) ++ "\" cy=\"" ++
        show ((freqDomainAmp !! f) * freqDomainScale)  ++ "\" r=\"" ++
        fromMaybe "1" (show <$> strokeWidthMaybe graphStroke) ++ "\"" ++
        (fromMaybe "" ((\s -> " fill=\"" ++ s ++ "\"") <$> (strokeColorMaybe graphStroke))) ++ "/>" ++
        "</" ++ maybePrefix prefix ++ "g>" ++
        epilogue prefix
    )
  for_ (zip3 (animateFreq (map realToComplex signal) f (_N `div` f)) [0..] signal)
    (\(content, num, s) ->
      writeFile ("graph" ++ show num ++ ".svg") (
        prologue prefix "xlink" (Just (WidthHeight "600" "600")) (Just $ ViewBox "-5" "-5" "10" "10") ++
        content ++
        "<" ++ maybePrefix prefix ++ "g transform=\"" ++ signalGraphTransform ++ "\">" ++
        "<" ++ maybePrefix prefix ++ "circle cx=\"" ++ show ((fromIntegral num) * graphScale) ++ "\" cy=\"" ++ show s ++ "\" r=\"" ++
        fromMaybe "1" (show <$> strokeWidthMaybe graphStroke) ++ "\"" ++
        (fromMaybe "" ((\s -> " fill=\"" ++ s ++ "\"") <$> (strokeColorMaybe graphStroke))) ++ "/>" ++
        "</" ++ maybePrefix prefix ++ "g>" ++
        epilogue prefix))
  

freqDomainScale :: RealFloat a => a
freqDomainScale = 1 / (2 * sqrt (fromIntegral (length signal)))
  
animateFreq :: (RealFloat a, Show a) => [Complex a] -> Int -> Int -> [String]
animateFreq signal f sumTraceN = let
  functionValues = ((map (\(s, wp, sum) -> (s, wp, sum * (freqDomainScale :+ 0)))) . tail . reverse) (correlateSignalAtFreq signal f)
  lastN = lastNWindow sumTraceN functionValues in
  map partToString (zip functionValues lastN) where
    partToString ((s, wavePart, sum), lastNTuples) =
      let opacities = [1.0 - (fromIntegral i) / (fromIntegral sumTraceN)  | i <- [0..sumTraceN - 1]]
          lastComplexes = zip lastNTuples opacities
          lastComplexesDraws =
            map
            (\((_, _, a :+ b), opacity) ->
              "<" ++ maybePrefix prefix ++ "circle fill=\"blue\" cx=\"" ++ show a ++
              "\" cy=\"" ++ show b ++ "\" r=\"0.01\" opacity=\"" ++ show opacity ++
              "\" />"
            )
            lastComplexes
      in
      drawComplex sum prefix (Stroke (Just "blue") (Just 0.03)) ++
      foldr1 (++) lastComplexesDraws ++
      drawComplex wavePart prefix (Stroke (Just "green") (Just 0.02)) ++
      drawComplex s prefix (Stroke (Just "red") (Just 0.01))

