module Main     where

import           Control.Monad            (foldM, when)
import           Control.Monad.Random
import           Data.Array
import           Data.Maybe               (Maybe (..))
import           Data.Semigroup           ((<>))
import           Debug.Trace
import           Generate
import           Graphics.Rendering.Cairo
import qualified UI

data SandPainter = SandPainter
  { c :: ColorFn
  , g :: Double
  }


data Crack = Crack
  { x  :: Double
  , y  :: Double
  , t  :: Integer
  , sp :: SandPainter
  }

type CGrid = Array Integer Integer
type Model = (CGrid, [Crack])

maxCracks = 5

renderSetup = do
  (w,h) <- getSize
  pure $ do
    rectangle 0 0 (fromIntegral w ) (fromIntegral h )
    white 1 *> fill

initialModel ::RandGen (CGrid, [Crack])
initialModel = do
  (w,h) <- getSize
  grid <- cgrid (w * h - 1)
  cracks <- noNothings <$> traverse (\_ -> mkCrack grid) [1..3]
  pure (grid, cracks)

step :: (CGrid, [Crack]) -> RandGen (CGrid, [Crack])
step (g,cs) = do
  (g',z) <- foldM folder (g,[]) cs

  pure (g', z)
    where
      folder (g, acs) c = do
        let cCount = length cs
        (g', cs') <- move (cCount < maxCracks) g c
        pure (g', acs <> cs')



renderModel :: Model -> RandGen (Render [()])
renderModel (grid, cracks) = do
  rs <- traverse (renderCrack grid) cracks
  pure $ sequence rs



renderCrack :: CGrid -> Crack -> RandGen (Render ())
renderCrack g c@(Crack x y t sp) = do
  sand <- regionColor g c
  let z = 0.71
  dx <- getRandomR (-z,z)
  dy <- getRandomR (-z,z)
  pure $ do
    sand
    point black 0.4 (x+dx) (y+dy)
    -- rectangle (x + dx) (y + dy) 1 1
    -- black 0.40 *> fill



regionColor :: CGrid -> Crack -> RandGen (Render [()])
regionColor g c@(Crack x y t sp) = do
  (w, h) <- getSize
  let
    openSpace grid x y =
        (x >= 0 && x < w && y>=0 && y < h) && (grid ! (x + y * w) > 10000)
    calc rx ry =
        if openSpace g (round rx) (round ry)
          then calc (rx + 0.81 * sina (fromIntegral t)) (ry - 0.81 * cosa (fromIntegral t))
          else (rx, ry)
    (rx,ry) = calc (x + 0.81 * sina (fromIntegral t)) (y - 0.81 * cosa (fromIntegral t))
  renderSand sp rx ry x y



move :: Bool -> CGrid -> Crack -> RandGen (CGrid, [Crack])
move b g c@(Crack x y t sp@(SandPainter col spg)) = do
  newg <- getRandomR (-0.05, 0.05)
  (w,h) <- getSize
  let
    newg'
      | newg + spg > 1 = 1
      | newg + spg < 0 = 0
      | otherwise = newg + spg
  let
    x' = x + 0.33 * cosa (fromIntegral t)
    y' = y + 0.33 * sina (fromIntegral t)
    newSp = SandPainter col newg'
    newC = Crack x' y' t newSp
    z = 0.33
  cx <- round . (+x') <$> getRandomR (-z,z)
  cy <- round . (+y') <$> getRandomR (-z,z)




  let idx = cy * w + cx
  if cx >= 0 && cx < w && cy >= 0 && cy < h
  then
    if (g ! idx > 10000) || (abs (t - g ! idx) < 5)
    then do
      let g' = g // [(idx, t)]
      pure (g', [newC])
    else

      if abs (t - g ! idx) > 2
      then startNewCrack b g newC
      else pure (trace "noop" g, [newC])
  else startNewCrack b g newC


startNewCrack b g c = do
  old <- mkCrack g
  let
    cs = case old of
          Nothing -> []
          Just cc -> [cc]
  if b then do
    m <- mkCrack g
    case m of
      Nothing -> pure (g, cs)
      Just c' -> pure (g, c' : cs)
  else pure (g, cs)


renderSand :: SandPainter -> Double -> Double -> Double -> Double -> RandGen (Render [()])
renderSand (SandPainter c g) x y ox oy = do
  g' <-  (g+) <$> getRandomR (-0.05, 0.05)
  let
    g''
      | g<0 = 0
      | g > 1 = 1
      | otherwise = g
    grains = 64
    w = g'' / (grains - 1)
  pure $ traverse (drawGr grains w) [0..grains-1]
    where
      drawGr grains w i = do
        let a = 0.1001 - i / (grains * 10.0)
        point c a (ox + (x-ox) * sinsin (i * w) ) (oy + (y - oy )* sinsin (i*w))


noNothings :: [Maybe a] -> [a]
noNothings []           = []
noNothings (Nothing:xs) = noNothings xs
noNothings (Just x:xs)  = x : noNothings xs


emptyArrLst size =
  (\i -> (i, 10001))
    <$> [0 .. size]



cgrid :: Integer -> RandGen CGrid
cgrid size = do
  ixs <- getRandomRs (0,  size)
  vals <- getRandomRs (0, 360)
  pure $ array (0, size) (emptyArrLst size) // take 20 (zip ixs vals)

someColor :: RandGen ColorFn
someColor = uniform palette

mkCrack :: CGrid -> RandGen (Maybe Crack)
mkCrack g = do
  (w, h) <- getSize
  let isCrack (x,y) =
        g ! (y*w + x) < 10000
  xs <- getRandomRs (0, w - 1)
  ys <- getRandomRs (0, h - 1)
  let ixs = filter isCrack (xs `zip` ys)
  case ixs of
    [] -> pure Nothing
    (x,y):_ -> do
      p <- mkSandPainter
      r <- getRandomR (-2, 2)
      r' <- uniform [90 + r, 90 - r]
      let a = r' + g ! (y*w + x)

      pure $ Just (Crack ( fromIntegral x + 0.61*cosa (fromIntegral a)) (fromIntegral y + 0.61 * sina (fromIntegral a)) (a `mod` 360) p)

mkSandPainter :: RandGen SandPainter
mkSandPainter = do
  c <- someColor
  g <- getRandomR (0.05, 0.3)
  pure $ SandPainter c g

--palette = [hsva 2.8 0.76 0.33, hsva 200 0.9 0.5, hsva 129.1 1 0.2667, hsva 71.4 1 0.5804, hsva 52.9 1 0.8941, hsva 68.6 0.7 0.9608, hsva 186.4 0.6878 0.8039]
-- palette =
--   [ hsva 60.000 1.000 0.500
--   , hsva 120.000 1.000 0.500
--   , hsva 300.000 1.000 0.900
--   , hsva 45.098 1.000 0.700
--   , hsva 36.000 1.000 0.500
--
--   ]
palette =
  [ hsva 340.909 0.234 0.184
  , hsva 354.783 0.242 0.186
  , hsva 353.333 0.205 0.173
  , hsva 20.426 0.219 0.422
  , hsva 27.568 0.420 0.345
  , hsva 34.737 0.258 0.433
  , hsva 40.000 0.235 0.600
  , hsva 26.949 0.562 0.412
  , hsva 24.706 0.571 0.467
  , hsva 22.991 0.423 0.496
  , hsva 23.586 0.578 0.508
  , hsva 27.805 0.577 0.582
  , hsva 35.510 0.613 0.686
  , hsva 39.130 0.602 0.625
  , hsva 48.000 0.733 0.853
  , hsva 55.862 0.829 0.931
  , hsva 48.293 0.350 0.771
  , hsva 55.862 0.829 0.931
  , hsva 60.000 0.071 0.806
  , hsva 150.000 0.060 0.675
  , hsva 135.789 0.086 0.567
  , hsva 180.000 0.192 0.602
  , hsva 180.000 0.145 0.486
  ]

cosa = cos . (*(pi/180))
sina = sin . (*(pi/180))


sinsin = sin . sin

main :: IO ()
main = UI.main "Substrate" (World 800 600 1) renderSetup initialModel step renderModel
