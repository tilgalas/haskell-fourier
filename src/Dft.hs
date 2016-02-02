module Dft where

import Data.Complex

nthPartOfCycle :: Floating a => Int -> Int -> Int -> Complex a
nthPartOfCycle n f _N =
  cis (2 * pi * ((fromIntegral (n * f)) / (fromIntegral _N)))

correlateSignalAtFreq :: RealFloat a => [Complex a] -> Int -> [(Complex a, Complex a, Complex a)]
correlateSignalAtFreq signal f =
  let _N = length signal
      wavePart = [nthPartOfCycle i f _N | i <- [0.._N - 1]]
  in
   scanr (\(s, w) (_, _, sum) -> (s, w, sum + s * w)) (0 :+ 0, 0 :+ 0, 0 :+ 0) (reverse $ zip signal wavePart)


