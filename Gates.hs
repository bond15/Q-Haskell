module Gates where

-- ghcid -c "cabal repl"
import Data.Complex
import Linear
import Control.Lens


c0 = 0.0 :+ 0.0
c1 = 1.0 :+ 0.0

q0 = V2 c1 c0
q1 = V2 c0 c1

neg :: Complex Double -> Complex Double
neg c = (- realPart c) :+ imagPart c

id2 :: M22 (Complex Double)
id2 = identity

hadamard22 :: M22 (Complex Double)
hadamard22 = V2 (V2 c1 c1) 
                (V2 c1 (neg c1)) !!*  (1/(sqrt 2) :+ 0.0)

cnot44 :: M44 (Complex Double)
cnot44 = V4 (V4 c1 c0 c0 c0) 
             (V4 c0 c1 c0 c0) 
             (V4 c0 c0 c0 c1) 
             (V4 c0 c0 c1 c0)
