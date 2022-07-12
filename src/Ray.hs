{-# Language RankNTypes #-}
module Ray
    ( testRender
    , testRender'
    ) where

import Linear
import Codec.Picture
import Data.Function
import Data.Time
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Strategies (parVector, using, NFData)
import Control.Monad.State (State(..), evalState, put, get)
import qualified Control.Monad.State as MS
import Control.Monad.ST
import qualified Data.Vector.Storable.Mutable as VSM

import System.Random
import System.CPUTime (getCPUTime)
import Foreign hiding (rotate)-- (Storable(..))

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
type Quat = Quaternion Scalar

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

multisamples :: VS.Vector (Vec2)
multisamples = VS.zipWith V2 xs ys
  where
    n = 100
    (gx, gy) = split (mkStdGen 0xbeef)
    xs = VS.fromList . take n $ randomRs (0, 1) gx
    ys = VS.fromList . take n $ randomRs (0, 1) gy

randRM :: (Random a, RandomGen g) => (a, a) -> State g a
randRM range = do
  g <- get
  let (a, g') = randomR range g
  put g'
  pure a

scatter :: VU.Vector Vec3
scatter = evalState (VU.generateM (n*n) go) (mkStdGen 0xbeef)
  where
    n = 32
    rcpN = recip (fromIntegral n)
    go i = do
      let x = fromIntegral (i `mod` n) * rcpN
          y = fromIntegral (i `div` n) * rcpN
      u <- randRM (0, rcpN)
      v <- randRM (0, rcpN)
      pure $ canonicalToUnit (x+u) (y+v)

specP :: VU.Vector Float
specP = VU.fromList $ take (VU.length scatter + 1) (randomRs (0, 1) (mkStdGen 0xfeed))

rots :: VU.Vector Quat
rots = VU.fromList . map normalize . take (VU.length scatter * 2 + 1) . zipWith Quaternion res $ ims
  where
    ims = zipWith3 V3 xs ys zs
    res = randomRs (-1, 1) (mkStdGen 1234)
    xs = randomRs (-1, 1) (mkStdGen 0xa0aa)
    ys = randomRs (-1, 1) (mkStdGen 0xb0bb)
    zs = randomRs (-1, 1) (mkStdGen 0xc0cc)

scatter' :: VU.Vector Vec3
scatter' = VU.fromList . take 128 . fmap normalize . filter f . zipWith3 V3 xs ys $ zs
  where
    f v = let q = quadrance v in (q <= 1) && (q > 0.001)
    xs = randomRs (-1, 1) (mkStdGen 0xaaaa)
    ys = randomRs (-1, 1) (mkStdGen 0xbbbb)
    zs = randomRs (-1, 1) (mkStdGen 0xcccc)

testRender :: IO ()
testRender =  testRender' 320 180 --render "out/test.png" 320 180 defaultCamera

testRender' :: Int -> Int -> IO ()
testRender' x y = do
  tm <- getCurrentTime
  let path = "out/test_" <> timestamp <> ".png"
      timestamp = fmap subst $ show tm
      subst ':' = '.'
      subst ' ' = '_'
      subst  c  =  c
  el <- elapsed $ render path x y defaultCamera
  let px = x*y
  putStrLn $ show el <> " seconds"
  putStrLn $ show px <> " pixels, " <> show (1000 * el / fromIntegral px) <> " ms per pixel"

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
    sph = S (V3 0.6 0.9 (-0.8)) 0.4 1 --orange
    sphs =
      [ S (V3 1.3 3.2 (-0.5)) 1.8 0 --grey
      , S (V3 3.0 (-2) 0.1) 1.3 0 --grey
      , S (V3 5 1 3.5) 3 3 --green
      , S (V3 6 (-6) 2) 5 0 --grey
      , S (V3 8 (6) 2) 3 4 --blue
      , S (V3 0 0 (-1000)) 998 3 --green
      , S (V3 1.6 (-3) (-1.5)) 0.2 1 --orange
      , S (V3 1.8 (-3.2) (-1.3)) 0.3 1 --orange
      , S (V3 1.9 (-2.4) (-1.6)) 0.25 2 --purpl
      , S (V3 (-2.5) (0.7) 0.2) 0.27 4 --blue
      , S (V3 (-3.5) (0.8) (0.1)) 0.125 2 --purpl
      ]

    wrld = W {
      -- _worldShapes = buildSphereTree (sph NE.:| sphs),
      _worldShapes = flattenTree (buildSphereTree (sph NE.:| sphs))
      , _worldMaterials = VS.fromList [grey, orange, purpl, green, blue]
      , _worldLights = V.fromList [Directional (normalize $ V3 1 2 (-3)) (pure 0.9)]
      }
    rcpRes = recip $ fromIntegral <$> V2 w h
    cam = cam0{ _cameraAspect = fromIntegral w / fromIntegral h, _cameraPos = V3 (-4) 0 2 }
    cast x y = rgb8 . worldTest 16 0 (x * 1543 + y * 3079) wrld . camUvToRays $ (fromIntegral <$> V2 x (h - 1 - y)) * rcpRes
    camUvToRays uv = VS.map (\o -> cameraUvToRay cam (o * rcpRes + uv)) multisamples


{-# INLINE rgb8 #-}
rgb8 :: LinColour -> PixelRGB8
rgb8 lin = PixelRGB8 r g b
  where
    gamma = (** (1/2.2)) -- 'gamma correct'
    V3 r g b = byte . gamma <$> lin
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

instance Storable Material where
  peek ptr = do
    diff <- peek (castPtr ptr)
    em <- peek (ptr `plusPtr` sizeOf diff)
    mtl <- peek (ptr `plusPtr` (sizeOf diff + sizeOf em))
    rough <- peek (ptr `plusPtr` (sizeOf diff + sizeOf em + sizeOf mtl))
    pure $ Mtl diff em mtl rough
  poke ptr (Mtl diff em mtl rough) = do
    poke (castPtr ptr) diff
    poke (ptr `plusPtr` sizeOf diff) em
    poke (ptr `plusPtr` (sizeOf diff + sizeOf em)) mtl
    poke (ptr `plusPtr` (sizeOf diff + sizeOf em + sizeOf mtl)) rough
  sizeOf ~(Mtl diff em mtl rough) = sizeOf diff + sizeOf em + sizeOf mtl + sizeOf rough
  alignment _ = 0


data Hit = Hit
  { _hitPos :: Vec3
  , _hitNormal :: Vec3
  , _hitDistance :: Scalar
  , _hitMaterial :: Int
  } deriving (Eq, Ord, Show, Read)

instance Semigroup Hit where
  a <> b = if _hitDistance a <= _hitDistance b then a else b

data Sphere = S
  { _spherePos :: !Vec3
  , _sphereRadius :: !Scalar
  , _sphereMaterial :: !Int
  } deriving (Eq, Ord, Show, Read)
instance Storable Sphere where
  peek ptr = do
    p <- peek (castPtr ptr)
    r <- peek (ptr `plusPtr` sizeOf p)
    m <- peek (ptr `plusPtr` (sizeOf p + sizeOf r))
    pure $ S p r m
  poke ptr (S p r m) = do
    poke (castPtr ptr) p
    poke (ptr `plusPtr` sizeOf p) r
    poke (ptr `plusPtr` (sizeOf p + sizeOf r)) m
  sizeOf ~(S p r m) = sizeOf p + sizeOf r + sizeOf m
  alignment _ = 0

data World = W
  { -- _worldShapes :: !(VS.Vector Sphere)
    -- _worldShapes :: !SphereTree
    _worldShapes :: !(VS.Vector FlatTree)
  , _worldMaterials :: !(VS.Vector Material)
  , _worldLights :: !(V.Vector Light)
  } deriving (Eq, Ord, Show, Read)

data Light
  = Directional !Vec3 !LinColour
  deriving (Eq, Ord, Show, Read)

data Ray = Ray
  { _rayStart :: !Vec3
  , _rayDir :: !Vec3
  } deriving (Eq, Ord, Show, Read)
instance Storable Ray where
  peek ptr = do
    o <- peek (castPtr ptr)
    d <- peek (ptr `plusPtr` sizeOf o)
    pure $ Ray o d
  poke ptr (Ray o d) = do
    poke (castPtr ptr) o
    poke (ptr `plusPtr` sizeOf o) d
  sizeOf ~(Ray o d) = sizeOf o + sizeOf d
  alignment _ = 0


data Camera = Camera
  { _cameraPos :: !Vec3
  , _cameraTarget :: !Vec3
  , _cameraUp :: !Vec3
  , _cameraAspect :: !Scalar
  , _cameraFov :: !Scalar
  , _cameraNear :: !Scalar
  , _cameraRcpRes :: !Vec2
  } deriving (Eq, Show)

defaultCamera :: Camera
defaultCamera = Camera
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
mkRayFromTo from to = Ray { _rayStart = from, _rayDir = normalize (to - from)}

mkRayWithDir :: Vec3 -> Vec3 -> Ray
mkRayWithDir from = unsafeMkRayWithDir from . normalize

unsafeMkRayWithDir :: Vec3 -> Vec3 -> Ray
unsafeMkRayWithDir from d = Ray { _rayStart = from, _rayDir = d }

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
skyTest (Ray _ (V3 dx _ dz)) = sh + lerp' (realToFrac dz * 0.5 + 0.5) down up
  where
    down = V3 0.2 0.4 1.0 :: LinColour
    up   = V3 0.9 0.85 1.0 :: LinColour
    sh   = V3 (0.1 * realToFrac dx) 0 0:: LinColour

{-# INLINE saturate #-}
saturate :: (Num a, Ord a) => a -> a
saturate = min 1 . max 0



--worldTest1 :: Int -> Int -> World -> Ray -> LinColour
--worldTest1 n i w ray = case

worldTest :: Int -> Int -> Int -> World -> VS.Vector Ray -> LinColour
worldTest term dpth i w rays
  | term <= 0     = 0
  | rayCount == 0 = 0
  | otherwise     = clr -- V.sum clrs / fromIntegral (V.length rays)
  where
    rayCount  = VS.length rays
    clr = if rayCount == 1
            then let ray = VS.head rays in colour 0 ray (hits ray)
            else let clrs = VS.imap (\j ray -> colour j ray (hits ray)) rays `using` (parVector 20)
                 in VS.sum clrs / fromIntegral (VS.length clrs)
    ix = abs i `mod` VU.length scatter
    i' = i * 6151
    trace ray (S p r m) = sphereRayTest ray p r m
    hits ray = flatTreeRayTest ray (_worldShapes w)
    getRot j = VU.unsafeIndex rots ((j*1543+i*389) `mod` VU.length rots)
    getDir j = rotate (getRot j) $ VU.unsafeIndex scatter ((j*769) `mod` VU.length scatter)
    --getDir j = VU.unsafeIndex scatter ((j*769) `mod` VU.length scatter)
    colour _ ray Nothing  = skyTest ray
    colour j ray (Just h)
      | dpth == 0 = (bounceSpec + anSpec) * specClr ^* fres + (bounceDiff + anDiff) * diffClr ^* (1-fres) + emit
      | sp < fres = (bounceSpec + anSpec) * specClr + emit
      | otherwise = (bounceDiff + anDiff) * diffClr + emit
      where
        (anDiff, anSpec) = analytic ray h mtl w
        emit = _mtlEmissive mtl
        diffClr = _mtlDiffuse mtl
        specClr = _mtlMetallic mtl *^ lerp' schlick diffClr 0.99 + pure (1 - _mtlMetallic mtl)
        sp = VU.unsafeIndex specP ((abs i * 1543 + j * 49157) `mod` VU.length specP)
        mtl = _worldMaterials w `VS.unsafeIndex` _hitMaterial h
        n_dot_i =  saturate . abs . realToFrac $ _rayDir ray `dot` _hitNormal h
        f_nonmetal = 0.04
        f0 = saturate (_mtlMetallic mtl + f_nonmetal) -- this is wrong but eh
        schlick = (1-n_dot_i)^(5::Int)
        fres = schlick * (1 - f0) + f0
        next = worldTest (pred term) (succ dpth) (i' + j) w
        bounceSpec = next . VS.singleton . makeSpecBounce ray (_mtlRoughness mtl) h $ getDir (ix+j)
        bounceDiff = next . VS.singleton . makeDiffBounce h $ getDir (ix+j)

analytic :: Ray -> Hit -> Material -> World -> (LinColour, LinColour)
analytic eye hit mtl w = V.foldl' (\ds l -> analyticLight eye hit mtl w l ds) (0,0) (_worldLights w)

analyticLight :: Ray -> Hit -> Material -> World -> Light -> (LinColour, LinColour) -> (LinColour, LinColour)
analyticLight eye hit mtl w (Directional dir clr) ds@(diff, spc) =
  case flatTreeRayTest (unsafeMkRayWithDir (_hitPos hit) (negate dir)) (_worldShapes w) of
    Just h -> ds
    Nothing -> (diff + clr ^* diffTerm, spc + clr ^* specTerm)
      where
        diffTerm = realToFrac $ saturate (_hitNormal hit `dot` dir)
        specTerm = diffTerm * spcPow * (abs (realToFrac (dir `dot` halfVec)) ** spcPow)
        spcPow = 1 + _mtlRoughness mtl * 29
        halfVec = normalize (_rayDir eye + _hitNormal hit)

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

sphereRayTest :: Ray -> Vec3 -> Scalar -> Int -> Maybe Hit
sphereRayTest (Ray o d) pos radius mtl
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

data BvhType
  = BvhNodes
  | BvhSphere
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data BvhNode = BvhNode
  { _bvhAabbMin :: !Vec3
  , _bvhAabbMax :: !Vec3
  , _bvhChildData :: !Int
  } deriving (Eq, Ord, Show)

instance Storable BvhNode where
  peek ptr = do
    mn <- peek (castPtr ptr)
    mx <- peek (ptr `plusPtr` sizeOf mn)
    dt <- peek (ptr `plusPtr` (sizeOf mn + sizeOf mx))
    pure $ BvhNode mn mx dt
  poke ptr (BvhNode mn mx dt) = do
    poke (castPtr ptr) mn
    poke (ptr `plusPtr` sizeOf mn) mx
    poke (ptr `plusPtr` (sizeOf mn + sizeOf mx)) dt
  sizeOf ~(BvhNode mn mx dt) = sizeOf mn + sizeOf mx + sizeOf dt
  alignment _ = 0

data BvhTree = BvhTree
  { _bvhTreeNodes :: !(VS.Vector BvhNode)
  , _bvhTreeSpheres :: !(VS.Vector Sphere)
  } deriving (Eq, Ord, Show)

bvhRayTest :: Ray -> BvhTree -> Maybe Hit
bvhRayTest ray bvh@(BvhTree nodes _)
  | VS.null nodes = Nothing
  | otherwise = bvhRayRecur ray (VS.head nodes) bvh

bvhRayRecur :: Ray -> BvhNode -> BvhTree -> Maybe Hit
bvhRayRecur ray node bvh
  | not (bvhRayNode ray node) = Nothing
  | otherwise = case bvhChildType node of
    BvhNodes ->
      let nodes = VS.slice (bvhChildIndex node) (bvhChildCount node) (_bvhTreeNodes bvh)
      in VS.foldl' (\hit node' -> hit <> bvhRayRecur ray node' bvh) Nothing nodes
    BvhSphere ->
      case bvhChildCount node of
        1 -> let sph = _bvhTreeSpheres bvh VS.! bvhChildIndex node in sphereRayTest ray (_spherePos sph) (_sphereRadius sph) (_sphereMaterial sph)
        count ->
          let sphs = VS.slice (bvhChildIndex node) count (_bvhTreeSpheres bvh)
          in VS.foldl' (\hit sph -> hit <> sphereRayTest ray (_spherePos sph) (_sphereRadius sph) (_sphereMaterial sph)) Nothing sphs


-- ray/aabb intersection
bvhRayNode :: Ray -> BvhNode -> Bool
bvhRayNode ray bvh = undefined

bvhChildIndex :: BvhNode -> Int
bvhChildIndex = (.&. 0x00000000ffffffff) . _bvhChildData

setChildIndex :: Int -> BvhNode -> BvhNode
setChildIndex idx bvh = BvhNode (_bvhAabbMin bvh) (_bvhAabbMax bvh) payload'
  where
    payload = _bvhChildData bvh
    payload' = (payload .&. complement mask) .|. (idx .&. mask)
    mask = 0x00000000ffffffff



bvhChildCount :: BvhNode -> Int
bvhChildCount = (`shiftR` 32) . (.&. 0x0000ffff00000000) . _bvhChildData

bvhChildType :: BvhNode -> BvhType
bvhChildType = toEnum . (.&. 0x0000ffff) . (`shiftR` 48) . _bvhChildData

bvhNodeVolume :: BvhNode -> Scalar
bvhNodeVolume (BvhNode mn mx _) = product (mx - mn)

makeSphereNode :: Int -> Sphere -> BvhNode
makeSphereNode idx (S pos rad _mtl) = BvhNode
  { _bvhAabbMin = pos - pure rad
  , _bvhAabbMax = pos + pure rad
  , _bvhChildData =
      (fromEnum BvhSphere `shiftL` 48) .|.
      (1 `shiftL` 32) .|.
      (idx .&. 0xffffffff)
  }

sphereSingleton :: Sphere -> BvhTree
sphereSingleton sph = BvhTree
  { _bvhTreeNodes = VS.singleton (makeSphereNode 0 sph)
  , _bvhTreeSpheres = VS.singleton sph
  }

--
sphereVolume :: Sphere -> Scalar
sphereVolume (S _ r _) = r*r*r*4/3*pi

data SphereTree
  = SphereNode !Sphere SphereTree SphereTree
  | SphereLeaf !Sphere
  deriving (Eq, Ord, Show, Read)

sphereTreeSphere :: SphereTree -> Sphere
sphereTreeSphere (SphereNode s _ _) = s
sphereTreeSphere (SphereLeaf s) = s

sphereTreeVolume :: SphereTree -> Scalar
sphereTreeVolume = sphereVolume . sphereTreeSphere

instance Semigroup Sphere where
  (S pa ra ma) <> (S pb rb mb)
    | nearZero (pa - pb) = S pa (max ra rb) (ma .|. mb)
    | rb < ra && qd pb pa <= (ra-rb)*(ra-rb) = S pa ra (ma .|. mb)
    | ra < rb && qd pb pa <= (rb-ra)*(rb-ra) = S pb rb (ma .|. mb)
    | otherwise = S p r (ma .|. mb)
    where
      d = normalize (pb - pa)
      pa' = (pa - d ^* ra)
      p = 0.5 *^ (pa' + (pb + d ^* rb))
      r = distance pa' p

instance Semigroup SphereTree where
  a <> b = SphereNode (sphereTreeSphere a <> sphereTreeSphere b) a b

sphereTreeSingleton :: Sphere -> SphereTree
sphereTreeSingleton = SphereLeaf

sphereTreeRayTest :: Ray -> SphereTree -> Maybe Hit
sphereTreeRayTest ray (SphereLeaf (S p r m)) = sphereRayTest ray p r m
sphereTreeRayTest ray (SphereNode (S p r m) left right) = case sphereRayTest ray p r m of
  Just _ -> sphereTreeRayTest ray left <> sphereTreeRayTest ray right
  Nothing -> Nothing

sphereTreeSize :: SphereTree -> Int
sphereTreeSize (SphereLeaf _) = 1
sphereTreeSize (SphereNode _ l r) = 1 + sphereTreeSize l + sphereTreeSize r

buildSphereTreeMb :: [Sphere] -> Maybe SphereTree
buildSphereTreeMb [] = Nothing
buildSphereTreeMb (x:xs) = Just $ buildSphereTree (x NE.:| xs)

buildSphereTree :: NE.NonEmpty Sphere -> SphereTree
buildSphereTree = reduceSphereTree . fmap sphereTreeSingleton

reduceSphereTree :: NE.NonEmpty SphereTree -> SphereTree
reduceSphereTree sphs = case sphs of
  (s NE.:| []) -> s
  (s NE.:| [o]) -> s <> o
  (s NE.:| ss) -> reduceSphereTree $ s' NE.:| fmap snd (tail ss')
    where
      ss' = L.sortBy (compare `on` (sphereTreeVolume.fst)) (fmap (\other -> ((other <> s),other)) ss)
      s' = fst (head ss')


data FlatTree = FlatTree
  { _flatTreeSphere :: !Sphere
  , _flatTreeChildren :: !Int
  } deriving (Eq, Ord, Show, Read)

flatTreeLeft, flatTreeRight :: FlatTree -> Int
flatTreeLeft ft = _flatTreeChildren ft .&. 0x00000000ffffffff
flatTreeRight ft = (_flatTreeChildren ft `shiftR` 32) .&. 0x00000000ffffffff

flatTreeChildren :: FlatTree -> (Int, Int)
flatTreeChildren ft = (flatTreeLeft ft, flatTreeRight ft)

makeFlatTree :: Sphere -> Int -> Int -> FlatTree
makeFlatTree s l r = FlatTree s (l .|. (r `shiftL` 32))

instance Storable FlatTree where
  peek ptr = do
    s <- peek (castPtr ptr)
    c <- peek (ptr `plusPtr` sizeOf s)
    pure $ FlatTree s c
  poke ptr (FlatTree s c) = do
    poke (castPtr ptr) s
    poke (plusPtr ptr (sizeOf s)) c
  sizeOf _ = sizeOf (undefined :: Sphere) + sizeOf (undefined :: Int )
  alignment _ = 0

flattenTree :: SphereTree -> VS.Vector FlatTree
flattenTree (SphereLeaf s) = VS.singleton (FlatTree s (-1))
flattenTree node = runST (VSM.new (sphereTreeSize node) >>= \vs -> flattenTreeST vs 0 node *> VS.unsafeFreeze vs)

flattenTreeST :: forall s. VSM.MVector s FlatTree -> Int -> SphereTree -> ST s Int
flattenTreeST vs ix sph = case sph of
  SphereLeaf s -> VSM.unsafeWrite vs ix (FlatTree s (-1)) *> pure (succ ix)
  SphereNode s l r -> do
    ln <- flattenTreeST vs (succ ix) l
    VSM.unsafeWrite vs ix (makeFlatTree s (succ ix) ln)
    flattenTreeST vs ln r

flatTreeRayTest :: Ray -> VS.Vector FlatTree -> Maybe Hit
flatTreeRayTest ray fts = go 0
  where
    go ix = case sphereRayTest ray p r m of
        h@Just{}
          | _flatTreeChildren ft == -1 -> h
          | otherwise -> go left <> go right
        n@Nothing -> n
      where
        ft = VS.unsafeIndex fts ix
        (S p r m) = _flatTreeSphere ft
        (left, right) = flatTreeChildren ft





--
