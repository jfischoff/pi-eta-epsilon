{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Lanugage.PiEtaEpsilon.Visualizer.Render where
import Diagrams.Core
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.VectorSpace
import Diagrams.Coordinates

data Typ 
   = Typ :+: Typ
   | Typ :*: Typ
   | Recip  Typ
   | Negate Typ
   | One
   | Zero
         

renderType :: Typ -> Diagram SVG R2
renderType e = to e where
   to t = case t of
      x :+: y  -> plusDiagram  (to x) (to y)
      x :*: y  -> timesDiagram (to x) (to y)
      Recip x  -> recipDiagram (to x)
      Negate x -> negateDiagram (to x)
      One      -> oneDiagram
      Zero     -> zeroDiagram
      
plusDiagram  = (|||)
timesDiagram = (===)
recipDiagram = undefined
negateDiagram = undefined

oneDiagram :: Diagram SVG R2 
oneDiagram = 
   circle 0.5 
   === 
   straightLine
   
straightLine = stroke . fromSegments . (:[]) . straight $ 0.1

line :: P2 -> P2 -> Gram
line start end = stroke (fromVertices [start, end]) # lw 0.1

line' x y = fromVertices [x, y]

 
zeroDiagram  = undefined

sizeSpec = Dims 1000 1000

circle0 :: Diagram SVG R2
circle0 = circle 1.0 # lw 0.01 # fc pink

d :: Diagram SVG R2 -> IO ()
d = TL.writeFile "output.svg" . toSVG

d1 :: Diagram SVG R2 -> IO ()
d1 x = d $ x # lw 0.1 # fc pink
   
toSVG x = renderSvg $ 
   renderDia SVG (SVGOptions sizeSpec) x
   
type Gram = Diagram SVG R2

cap :: Gram -> Gram -> Gram
cap lab op = position [(p2 (0, 0.3), freeze lab # scale 0.2), 
                (p2 (0,   0), hrule 1.0   # lw    0.1)] ||| 
      arc (0.25 :: CircleFrac) 0.75 # lw 0.1 |||
      position [(p2 (0,  1.0), hrule 1.0 # lw 0.1), 
                (p2 (0,  0.0), freeze op # scale 0.5),
                (p2 (0, -1.0), hrule 1.0 # lw 0.1)  ]

sumCap   = cap zero plus
timesCap = cap one cross

plus :: Diagram SVG R2
plus = vrule 1.0 # lw 0.1 `atop` hrule 1.0 # lw 0.1

zero :: Diagram SVG R2
zero = circle 1 # lw 0.1 # scaleX 0.5

cross :: Gram
cross = undefined

one :: Gram 
one = hrule 1 # lw 0.1

data OneD = OneD {
      branch    :: Double,
      input     :: P2,
      output1   :: P2,
      outputVar :: P2
   }
   
data Mode = Introduce
          | Eliminate

renderOneD :: Mode -> OneD -> Gram
renderOneD m o = case m of
   Introduce -> renderOneD' o
   Eliminate -> renderOneD' o # scaleX (-1)
   
arcC :: (PathLike p, V p ~ R2) => CircleFrac -> CircleFrac -> p
arcC start end = arc start end

lineP :: P2 -> P2 -> Double -> P2
lineP start end t = start .+^ t *^ (end .-. start)

renderOneD' :: OneD -> Gram
renderOneD' (OneD {..}) = position [
      (0 & 0, line input outputVar),
      (branchStart, alignBL $ 
         roundedCorner VUp HLeft branchWidth branchHeight (branchHeight / 4)  # lw 0.1)
   ] where
      branchStart  = lineP input outputVar branch
      branchWidth  = magnitude $ outputVar .-. branchStart
      branchHeight = magnitude $ project unitY $ output1 .-. origin

data VerticalDirection   = VUp | VDown
data HorizontalDirection = HLeft | HRight

roundedCorner vd hd w h r
   = trans vd hd . pathLike (p2 (w/2, abs r - h/2)) False
   . trailSegments
   $ seg (0, h)
   <> mkCorner 0 r
   <> seg (r -w, 0)
  where seg   = fromOffsets . (:[]) . r2
        mkCorner k r | r == 0    = mempty
                     | r < 0     = doArc 3 2
                     | otherwise = doArc 0 1
                     where doArc d d' = arc' r ((k+d)/4) ((k+d')/4:: CircleFrac)
                     
        trans vd hd x = case (vd, hd) of
           (VUp  , HRight) -> x 
           (VUp  , HLeft ) -> x # scaleX (-1)
           (VDown, HRight) -> x # scaleY (-1)
           (VDown, HLeft ) -> x # scaleX (-1) # scaleY (-1)

testOneD = OneD 0.5 (p2 (0, 0)) (p2 (1, 0.25)) (p2 (1.0, 0.0))

flipY x = x # scaleY (-1)
   
lineCross = onePath <> flipY onePath  where
   onePath = fromOffsets [1 & 0, 1 & 1, 1 & 0] # translateY (-0.5) 

triangle p0 p1 p2 = close $ fromVertices [p0, p1, p2]

distribute = result where
   result        = (bLine <> b1Plusb2Line <> 
                     tri) ||| ((topLines # translateY 1) <> (bottomLines # translateY (-1)))
   bLine         = hrule 1.0 # translateX (-0.5)
   b1Plusb2Line  = hrule 1.1 # translate ((-0.45) & 0.2)
   tri           = triangle (0 & 0) (0.3 & 1) (0.3 & (-1)) 
   topLines      = hrule 0.4 <> (hrule 0.4 # translateY (-0.3))
   bottomLines   = hrule 0.4 <> (hrule 0.4 # translateY 0.3)


traceDia = result where
   result  = box <> loop # translateY 0.4 <> b2Line <> b3Line 
   box     = rect 3.0 0.5 
   b2Line  = hrule 1.0 # translate ((-2) & (-0.1))
   b3Line  = hrule 1.0 # translate (2 & (-0.1))
   loop    = capsule 4 0.5
   
capsule w h = roundedRect w h h

pairOfLines = hrule 1 # translateY 0.5 <> hrule 1 # translateY (-0.5)






























