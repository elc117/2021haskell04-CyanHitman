import Text.Printf
   
type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)

greenPalette :: Int -> [(Int,Int,Int)]
greenPalette 8 = [(0, 80+i*10, 0) | i <- [0..8] ]

rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(70,130,180),(148,0,211),(255,255,0)]

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [((m*(w+gap), 12.5), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (86,79)
        gap = 20


svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

svgEnd :: String
svgEnd = "</svg>"

svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

main :: IO ()
main = do
  writeFile "figs.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsInLine nrects
        palette = rgbPalette nrects
        nrects = 14
        (w,h) = (1400,700)

