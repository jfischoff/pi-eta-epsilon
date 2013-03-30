{-# LANGUAGE NoMonomorphismRestriction #-}
module Lanugage.PiEtaEpsilon.Visualizer.Visualizer where
import Language.PiEtaEpsilon.Syntax
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

main = do 
    let sizeSpec = Dims (fromIntegral 100) (fromIntegral 200)
        outTy = PNG
    fst $ renderDia Cairo (CairoOptions "test.png"
                                    sizeSpec
                                    outTy
                                 ) $ stroke $ circle 1.0 

                
        
