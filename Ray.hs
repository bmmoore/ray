import Control.Monad
import Data.Maybe
import Data.Ord
import Data.List
import System.IO

data Vec = Vec !Double !Double !Double
  deriving Show

dot :: Vec -> Vec -> Double
dot (Vec x1 x2 x3) (Vec y1 y2 y3) = x1*y1 + x2*y2 + x3*y3

scale s (Vec x1 x2 x3) = Vec (s*x1) (s*x2) (s*x3)

norm v = sqrt (dot v v)
normalize v = scale (1/norm v) v

diff (Vec x1 x2 x3) (Vec y1 y2 y3) = Vec (x1-y1) (x2-y2) (x3-y3)
add  (Vec x1 x2 x3) (Vec y1 y2 y3) = Vec (x1+y1) (x2+y2) (x3+y3)

data Sphere = Sphere !Double !Vec !Double !Double

hit :: Vec -> Vec -> Sphere -> Maybe (Double, Vec, Vec)
hit x d (Sphere r center _ _) = do
  let face = diff x center
      a = dot d d
      b = 2*dot d face
      c = (dot face face) - (r*r)
      disc = b^2 - 4*a*c
  guard (b < 0) -- vector from center to camera should point opposite from ray
  guard (disc >= 0)
  let t = 2*c / (-b + sqrt disc)
      intersection = x `add` scale t d
      normal = normalize (diff intersection center)
      xbounce = dot d normal
      reflected = add d (scale (-2*xbounce) normal)
  return (t, intersection, reflected)

skyColor d = let (Vec _ y _) = normalize d in (y+1)/2

blend alpha x y = alpha*x + (1-alpha)*y

trace :: [Sphere] -> Int -> Vec -> Vec -> Double
trace _ 0 _ d = skyColor d
trace spheres n x d =
  case catMaybes [fmap (\x -> (x,s)) (hit x d s) | s <- spheres] of
    [] -> skyColor d
    hits -> let ((_,hit,normal),(Sphere _ _ alpha color)) = minimumBy (comparing (\((d,_,_),_)->d)) hits
            in blend alpha color (trace spheres (n-1) hit normal)

sample i n = fromIntegral i / fromIntegral (n-1)

raster :: Vec -> Vec -> Vec -> Vec -> Int -> Int -> [[(Vec,Vec)]]
raster origin forward up right px py =
  [let cy = 2*(sample iy py)-1 in
  [let cx = 2*(sample ix px)-1 in
    let vec = forward `add` scale cy up `add` scale cx right
    in (origin {-`add` vec-}, vec)
  | ix <- [0..px-1]] | iy <- [0..py-1]]
  
pgmish :: Int -> Int -> [[Double]] -> String
pgmish x y img = unlines ["P5",unwords [show x, show y],"65535"] ++ concatMap (concatMap pix) img
  where pix d = bytes . max 0 . min 65535 $ floor (d * 65535)
        bytes n = [toEnum (n `quot` 256), toEnum (n `rem` 256)]
        
main =
  withBinaryFile "out.pgm" WriteMode $ \h -> hPutStr h $
    pgmish 400 400 (map (map (uncurry (trace scene 10))) (raster (Vec 0 0 0) (Vec 0 0 1) (Vec 0 0.5 0) (Vec 0.5 0 0) 400 400))
 where scene = [(Sphere 10 (Vec 0 0 30)) 0.1 1,
                (Sphere 5 (Vec 10 0 30)) 0.1 0,
                (Sphere 10 (Vec 0 20 0)) 0.1 0.5]
