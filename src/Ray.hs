module Ray
    ( testRender
    , testRender'
    ) where

import Linear
import Codec.Picture
import Data.Time
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Strategies (parVector, using, NFData)
import Control.Monad.State (State(..), evalState, put, get)
import qualified Control.Monad.State as MS

import System.Random
import System.CPUTime (getCPUTime)

elapsed :: IO () -> IO Double
elapsed body = do
  t0 <- getCPUTime
  body
  t1 <- getCPUTime
  pure $ fromIntegral (t1-t0) * 1e-12

type Scalar = Double
type Vec2 = V2 Scalar
type Vec3 = V3 Scalar
type Mat3 = M33 Scalar
type LinColour = V3 Float

fract :: RealFrac a => a -> a
fract x = abs (x - fromIntegral i)
  where i = floor x :: Int

-- | Create a unit vector based on two uniformly distributed numbers in the range [0,1)
canonicalToUnit :: Scalar -> Scalar -> V3 Scalar
canonicalToUnit u v
  | u < 0 || u > 1 || v < 0 || v > 1 = canonicalToUnit (fract u) (fract v)
  | otherwise = V3 x y z
  where
    z = u * 2 - 1
    u' = sqrt (1 - z*z)
    th = v * 2 * pi
    y = u' * sin th
    x = u' * cos th

multisamples :: V.Vector (Vec2)
multisamples = V.zipWith V2 xs ys
  where
    n = 100
    (gx, gy) = split (mkStdGen 0xbeef)
    xs = V.fromList . take n $ randomRs (0, 1) gx
    ys = V.fromList . take n $ randomRs (0, 1) gy

randRM :: (Random a, RandomGen g) => (a, a) -> State g a
randRM range = do
  g <- get
  let (a, g') = randomR range g
  put g'
  pure a

scatter :: V.Vector Vec3
scatter = evalState (V.generateM (n*n) go) (mkStdGen 0xbeef)
  where
    n = 5
    rcpN = recip (fromIntegral n)
    go i = do
      let x = fromIntegral (i `mod` n) * rcpN
          y = fromIntegral (i `div` n) * rcpN
      u <- randRM (0, rcpN)
      v <- randRM (0, rcpN)
      pure $ canonicalToUnit (x+u) (y+v)

specP :: VU.Vector Float
specP = VU.fromList $ take (V.length scatter + 1) (randomRs (0, 1) (mkStdGen 0xfeed))

rots :: VU.Vector (Quaternion Scalar)
rots = VU.fromList . map normalize . take (V.length scatter * 2 + 1) . zipWith Quaternion res $ ims
  where
    ims = zipWith3 V3 xs ys zs
    res = randomRs (-1, 1) (mkStdGen 1234)
    xs = randomRs (-1, 1) (mkStdGen 0xa0aa)
    ys = randomRs (-1, 1) (mkStdGen 0xb0bb)
    zs = randomRs (-1, 1) (mkStdGen 0xc0cc)

scatter' :: V.Vector Vec3
scatter' = V.fromList . take 128 . fmap normalize . filter f . zipWith3 V3 xs ys $ zs
  where
    f v = let q = quadrance v in (q <= 1) && (q > 0.001)
    xs = randomRs (-1, 1) (mkStdGen 0xaaaa)
    ys = randomRs (-1, 1) (mkStdGen 0xbbbb)
    zs = randomRs (-1, 1) (mkStdGen 0xcccc)

testRender :: IO ()
testRender = render "out/test.png" 320 180 defaultCamera

testRender' :: Int -> Int -> IO ()
testRender' x y = do
  tm <- getCurrentTime
  let path = "out/test_" <> timestamp <> ".png"
      timestamp = fmap subst $ show tm
      subst ':' = '.'
      subst ' ' = '_'
      subst  c  =  c
  el <- elapsed $ render path x y defaultCamera
  putStrLn $ show el <> " seconds"

render :: FilePath -> Int -> Int -> Camera -> IO ()
render path w h cam0 = writePng path img
  where
    img :: Image PixelRGB8
    img = generateImage cast w h
    grey  = Mtl {_mtlDiffuse = 0.5, _mtlEmissive = 0, _mtlMetallic = 0, _mtlRoughness = 1}
    orange = Mtl {_mtlDiffuse = V3 1 0.4 0.1, _mtlEmissive = V3 3.5 1.1 0.1, _mtlMetallic = 0, _mtlRoughness = 1}
    purpl  = Mtl {_mtlDiffuse = V3 0.6 0.2 0.9, _mtlEmissive = V3 1.2 0.1 2.1, _mtlMetallic = 0, _mtlRoughness = 0.5}
    green = Mtl {_mtlDiffuse = V3 0.35 0.65 0.30, _mtlEmissive = 0, _mtlMetallic = 1, _mtlRoughness = 0.005}
    blue = Mtl {_mtlDiffuse = V3 0.25 0.55 0.90, _mtlEmissive = 0, _mtlMetallic = 1, _mtlRoughness = 0.04}
    wrld = W [ S (V3 0.6 0.9 (-0.8)) 0.4 orange
             , S (V3 1.3 3.2 (-0.5)) 1.8 grey
             , S (V3 3.0 (-2) 0.1) 1.3 grey
             , S (V3 5 1 3.5) 3 green
             , S (V3 6 (-6) 2) 5 grey
             , S (V3 8 (6) 2) 3 blue
             , S (V3 0 0 (-1000)) 998 green
             , S (V3 1.6 (-3) (-1.5)) 0.2 orange
             , S (V3 1.8 (-3.2) (-1.3)) 0.3 orange
             , S (V3 1.9 (-2.4) (-1.6)) 0.25 purpl
             , S (V3 (-2.5) (0.7) 0.2) 0.27 blue
             , S (V3 (-3.5) (0.8) (0.1)) 0.125 purpl
             ]
    rcpRes = recip $ fromIntegral <$> V2 w h
    cam = cam0{ _cameraAspect = fromIntegral w / fromIntegral h, _cameraPos = V3 (-4) 0 2 }
    cast x y = rgb8 . worldTest 16 0 (x * 1543 + y * 3079) wrld . camUvToRays $ (fromIntegral <$> V2 x (h - 1 - y)) * rcpRes
    camUvToRays uv = V.map (\o -> cameraUvToRay cam (o * rcpRes + uv)) multisamples


rgb8 :: LinColour -> PixelRGB8
rgb8 lin = PixelRGB8 r g b
  where
    V3 r g b = byte . sqrt <$> lin -- 'gamma correct'
    byte = truncate . (*255) . max 0 . min 1


-- Linear's version of lerp confuses and alarms me with argument order
{-# INLINE lerp' #-}
lerp' x b a = lerp x a b



data Material = Mtl
  { _mtlDiffuse :: !LinColour
  , _mtlEmissive :: !LinColour
  , _mtlMetallic :: !Float
  , _mtlRoughness :: !Float
  } deriving (Eq, Ord, Show, Read)

data Hit = Hit
  { _hitPos :: !Vec3
  , _hitNormal :: !Vec3
  , _hitDistance :: !Scalar
  , _hitMaterial :: !Material
  } deriving (Eq, Ord, Show, Read)

instance Semigroup Hit where
  a <> b = if _hitDistance a <= _hitDistance b then a else b

data Sphere = S
  { _spherePos :: !Vec3
  , _sphereRadius :: !Scalar
  , _sphereMaterial :: !Material
  } deriving (Eq, Ord, Show, Read)
data World = W
  { _worldShapes :: [Sphere]
  } deriving (Eq, Ord, Show, Read)

data Ray = R
    { _rayStart :: !Vec3
    , _rayDir :: !Vec3
    } deriving (Eq, Ord, Show, Read)


data Camera = C
  { _cameraPos :: !Vec3
  , _cameraTarget :: !Vec3
  , _cameraUp :: !Vec3
  , _cameraAspect :: !Scalar
  , _cameraFov :: !Scalar
  , _cameraNear :: !Scalar
  , _cameraRcpRes :: !Vec2
  } deriving (Eq, Show)

defaultCamera :: Camera
defaultCamera = C
  { _cameraPos = V3 (-4) 0 1
  , _cameraTarget = 0
  , _cameraUp = V3 0 0 1
  , _cameraAspect = 16 / 9
  , _cameraFov = pi/4
  , _cameraNear = 0.125
  , _cameraRcpRes = recip $ V2 160 90
  }

cameraNearSize :: Camera -> Vec2
cameraNearSize cam = V2 w h
  where
    tfov = tan $ _cameraFov cam
    h = tfov * _cameraNear cam
    w = _cameraAspect cam * h

cameraBasis :: Camera -> Mat3
cameraBasis cam = V3 x y z
  where
    z = normalize $ _cameraTarget cam - _cameraPos cam
    x = normalize $ z `cross` _cameraUp cam
    y = x `cross` z

mkRayFromTo :: Vec3 -> Vec3 -> Ray
mkRayFromTo from to = R { _rayStart = from, _rayDir = normalize (to - from)}

mkRayWithDir :: Vec3 -> Vec3 -> Ray
mkRayWithDir from = unsafeMkRayWithDir from . normalize

unsafeMkRayWithDir :: Vec3 -> Vec3 -> Ray
unsafeMkRayWithDir from d = R { _rayStart = from, _rayDir = d }

cameraUvToRay :: Camera -> Vec2 -> Ray
cameraUvToRay cam (V2 u v) = unsafeMkRayWithDir (_cameraPos cam) pixelDir
  where
    (V3 x y z) = cameraBasis cam
    (V2 w h) = cameraNearSize cam
    hz = x ^* w
    vt = y ^* h
    zn = z ^* _cameraNear cam
    bl = zn - (hz + vt) ^* 0.5
    pixelDir = normalize $ bl + (hz ^* u) + (vt ^* v)

{-# INLINE skyTest #-}
skyTest :: Ray -> LinColour
skyTest (R _ (V3 dx _ dz)) = sh + lerp' (realToFrac dz * 0.5 + 0.5) down up
  where
    down = V3 0.2 0.4 1.0 :: LinColour
    up   = V3 0.9 0.85 1.0 :: LinColour
    sh   = V3 (0.1 * realToFrac dx) 0 0:: LinColour

{-# INLINE saturate #-}
saturate :: (Num a, Ord a) => a -> a
saturate = min 1 . max 0



--worldTest1 :: Int -> Int -> World -> Ray -> LinColour
--worldTest1 n i w ray = case

worldTest :: Int -> Int -> Int -> World -> V.Vector Ray -> LinColour
worldTest term dpth i w rays
  | term <= 0     = 0
  | rayCount == 0 = 0
  | otherwise     = clr -- V.sum clrs / fromIntegral (V.length rays)
  where
    rayCount  = V.length rays
    clr = if rayCount == 1
            then let ray = V.head rays in colour 0 ray (hits ray)
            else let clrs = V.imap (\j ray -> colour j ray (hits ray)) rays `using` (parVector 20)
                 in V.sum clrs / fromIntegral (V.length clrs)
    ix = abs i `mod` V.length scatter
    i' = i * 6151
    trace ray (S p r m) = sphereRayTest ray p r m
    hits ray = case trace ray <$> _worldShapes w of
              (x:xs) -> foldl (<>) x xs
              [] -> Nothing
    --(qu:qv:qw:_) = randomRs (-1, 1) (mkStdGen (i * 49157 + n * 1543 + 6151))
    --q = normalize $ Quaternion qw (canonicalToUnit qu qv)
    getRot j = VU.unsafeIndex rots ((j*1543+i*389) `mod` VU.length rots)
    getDir j = rotate (getRot j) $ V.unsafeIndex scatter ((j*769) `mod` V.length scatter)
    colour _ ray Nothing  = skyTest ray
    colour j ray (Just h)
      | dpth == 0 = bounceSpec * specClr ^* fres + bounceDiff * diffClr ^* (1-fres) + emit
      | sp < fres = bounceSpec * specClr + emit
      | otherwise = bounceDiff * diffClr + emit
      where
        emit = _mtlEmissive mtl
        diffClr = _mtlDiffuse mtl
        specClr = _mtlMetallic mtl *^ lerp' schlick diffClr 0.99 + pure (1 - _mtlMetallic mtl)
        sp = VU.unsafeIndex specP ((abs i * 1543 + j * 49157) `mod` VU.length specP)
        mtl = _hitMaterial h
        n_dot_i = realToFrac . saturate . abs $ _rayDir ray `dot` _hitNormal h
        f_nonmetal = 0.04
        f0 = saturate (_mtlMetallic mtl + f_nonmetal) -- this is wrong but eh
        schlick = (1-n_dot_i)^(5::Int)
        fres = schlick * (1 - f0) + f0
        next = worldTest (pred term) (succ dpth) (i' + j) w
        bounceSpec = next . V.singleton . makeSpecBounce ray (_mtlRoughness mtl) h $ getDir (ix+j)
        bounceDiff = next . V.singleton . makeDiffBounce h $ getDir (ix+j)


makeDiffBounce :: Hit -> Vec3 -> Ray
makeDiffBounce hit d =  mkRayWithDir p (if nearZero o then n else o)
  where
    p = _hitPos hit -- + n ^* 0.001
    n = _hitNormal hit
    o = n + d

makeSpecBounce :: Ray -> Float -> Hit -> Vec3 -> Ray
makeSpecBounce incoming rough hit d = mkRayWithDir p (if nearZero o then n else o)
  where
    p = _hitPos hit -- + n ^* 0.001
    n = _hitNormal hit
    i = _rayDir incoming
    r = i - n ^* (2 * i `dot` n)
    o = r + d ^* realToFrac rough -- lerp' rough r (n + d)

{-# INLINE qd' #-}
qd' :: Vec3 -> Vec3 -> Scalar
qd' (V3 ax ay az) (V3 bx by bz) = cx*cx+cy*cy+cz*cz
  where
    cx = ax - bx
    cy = ay - by
    cz = az - bz

{-# INLINE (+/) #-}
(+/) :: Vec3 -> Vec3 -> Vec3
(V3 ax ay az) +/ (V3 bx by bz) = V3 (ax+bx) (ay+by) (az+bz)
infixl 6 +/

{-# INLINE (-/) #-}
(-/) :: Vec3 -> Vec3 -> Vec3
(V3 ax ay az) -/ (V3 bx by bz) = V3 (ax-bx) (ay-by) (az-bz)
infixl 6 -/

sphereRayTest :: Ray -> Vec3 -> Scalar -> Material -> Maybe Hit
sphereRayTest (R o d) pos radius mtl
  | distToRaySq > radSq = Nothing
  | param >= 0 = Just h
  | otherwise = Nothing
  where
    radSq = radius * radius
    l = pos -/ o
    ld = l `dot` d
    onRay = d ^* ld +/ o
    distToRaySq = qd' onRay pos
    delta = sqrt (radSq - distToRaySq)
    i0 = onRay -/ d ^* delta
    i1 = onRay +/ d ^* delta
    -- test if ray starts inside sphere
    i = if qd' o pos <= radSq then i1 else i0
    param = (i -/ o) `dot` d
    h = Hit { _hitPos = i
            , _hitNormal = normalize (i -/ pos)
            , _hitDistance = param
            , _hitMaterial = mtl}
































--
