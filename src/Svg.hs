module Svg where

import Data.Maybe
import Control.Applicative
import Data.Complex

data ViewBox = ViewBox {
  viewBoxX :: String,
  viewBoxY :: String,
  viewBoxWidth :: String,
  viewBoxHeight :: String
  } deriving Eq

viewBoxToString :: ViewBox -> String
viewBoxToString vb = "viewBox=\"" ++ viewBoxX vb ++ " " ++ viewBoxY vb ++ " " ++
                     viewBoxWidth vb ++ " " ++ viewBoxHeight vb ++ "\""

data WidthHeight = WidthHeight {
  width :: String,
  height :: String
  } deriving Eq

switchOrientation :: WidthHeight -> WidthHeight
switchOrientation (WidthHeight w h) = WidthHeight h w

widthHeightToString :: WidthHeight -> String
widthHeightToString (WidthHeight w h) = "width=\"" ++ w ++ "\" height=\"" ++ h ++ "\""

a4PortraitWidthHeight :: WidthHeight
a4PortraitWidthHeight = WidthHeight "210mm" "297mm"

data Stroke = Stroke {
  strokeColorMaybe :: Maybe String,
  strokeWidthMaybe :: Maybe Double
  } deriving Eq

strokeToString :: Stroke -> String
strokeToString (Stroke cm wm) = maybeToAttr cm colorToString ++
                                maybeToAttr wm widthToString where
                                  colorToString c = "stroke=\"" ++ c ++ "\""
                                  widthToString w = "stroke-width=\"" ++ show w ++ "\""
  

maybeToAttr :: Maybe a -> (a -> String) -> String
maybeToAttr maybe toStr =
  fromMaybe "" (((' ' :) . toStr) <$> maybe)

maybePrefix :: Maybe String -> String
maybePrefix m = fromMaybe "" ((++ ":") <$> m)

prologue :: Maybe String -> String -> Maybe WidthHeight -> Maybe ViewBox -> String 
prologue svgPrefixMaybe xlinkPrefix widthHeightMaybe viewBoxMaybe =
  "<" ++  maybePrefix svgPrefixMaybe  ++ 
  "svg xmlns" ++ (fromMaybe "" ((':' :) <$> svgPrefixMaybe)) ++
  "=\"http://www.w3.org/2000/svg\" xmlns:" ++
  xlinkPrefix ++ "=\"http://www.w3.org/1999/xlink\"" ++
  maybeToAttr widthHeightMaybe widthHeightToString ++
  maybeToAttr viewBoxMaybe viewBoxToString ++
  "><" ++ maybePrefix svgPrefixMaybe ++ "g transform=\"scale(1 -1)\">"
              
epilogue :: Maybe String -> String
epilogue svgPrefixMaybe = let pref = maybePrefix svgPrefixMaybe in
  "</" ++ pref ++ "g></" ++ pref ++ "svg>"

drawGraph :: [(Double, Double)] -> Maybe Int -> Maybe String -> Stroke -> String
drawGraph pnts highlightMaybe svgPrefixMaybe stroke =
  "<" ++ maybePrefix svgPrefixMaybe ++ "path" ++
  strokeToString stroke ++
  " fill=\"none\" d=\"M " ++ pairToString (head pnts) ++
  " L" ++ (foldr (\s1 s2 -> (' ' : s1) ++ s2) "" (map pairToString (tail pnts))) ++
  "\" />" ++ (fromMaybe "" ((\i -> let (cx, cy) = (pnts !! i) in
    ("<" ++ maybePrefix svgPrefixMaybe ++ "circle transform=\"translate(" ++ show cx ++ " " ++ show cy ++
     ") scale(" ++ show (256.0 / 8.0) ++ " 1)\" cx=\"0\" cy=\"0\" r=\"" ++
     fromMaybe "1" (show <$> strokeWidthMaybe stroke) ++ "\"" ++
     (fromMaybe "" ((\s -> " fill=\"" ++ s ++ "\"") <$> (strokeColorMaybe stroke))) ++ "/>")) <$> highlightMaybe))
  where
    pairToString (x, y) = show x ++ " " ++ show y


drawComplex :: Show a => Complex a -> Maybe String -> Stroke -> String
drawComplex (a :+ b) svgPrefixMaybe stroke =
  "<" ++ maybePrefix svgPrefixMaybe ++ "line" ++ strokeToString stroke ++
  " x1=\"0\" y1=\"0\" x2=\"" ++ show a ++ "\" y2=\"" ++ show b ++ "\" />"
  
