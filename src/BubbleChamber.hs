{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import           Control.Monad.Random
import qualified Data.Array               as A
import           Graphics.Rendering.Cairo

import           Generate
import qualified UI

data Muon = Muon
  { x       :: Double
  , y       :: Double
  , speed   :: Double
  , theta   :: Double
  , speedD  :: Double
  , thetaD  :: Double
  , thetaDD :: Double
  , myc     :: ColorFn
  , mya     :: ColorFn
  }
data Hadron = Hadron
  { x       :: Double
  , y       :: Double
  , speed   :: Double
  , theta   :: Double
  , speedD  :: Double
  , thetaD  :: Double
  , thetaDD :: Double
  , myc     :: ColorFn
  }

data Quark = Quark
  { x       :: Double
  , y       :: Double
  , speed   :: Double
  , theta   :: Double
  , speedD  :: Double
  , thetaD  :: Double
  , thetaDD :: Double
  , myc     :: ColorFn
  }

data Axion = Axion
  { x       :: Double
  , y       :: Double
  , speed   :: Double
  , theta   :: Double
  , speedD  :: Double
  , thetaD  :: Double
  , thetaDD :: Double
  }

data Universe = Universe
  { collisionTheta :: Double
  , muons          :: A.Array Integer Muon
  , axions         :: A.Array Integer Axion
  , hadrons        :: A.Array Integer Hadron
  , quarks         :: A.Array Integer Quark
  }



initialModel :: RandGen Universe
initialModel = do
  ct <- getRandomR (0, twoPi)
  let
    ml = A.listArray (0,300) [1..550]
    al = A.listArray (0,10) [1..11]
    hl = A.listArray (0,50) [1..100]
    ql = A.listArray (0,100) [1..339]
  ms <- traverse (const (collideMuon ct)) ml
  as <- traverse (const (collideAxion ct)) al
  hs <- traverse (const (collideHadron ct)) hl
  qs <- traverse (const (collideQuark ct)) ql
  pure $ Universe ct ms as hs qs





(!) :: [a] -> Int -> a
(!) xs n = head $ drop n xs



collideMuon :: Double -> RandGen Muon
collideMuon ct = do
  (w, h) <- getSize
  let
    x' = fromIntegral w / 2
    y' = fromIntegral h / 2
    thetaD' = 0
  speed' <- getRandomR (2, 32)
  speedD' <- getRandomR (0.0001, 0.001)
  theta' <- (+ct) <$> getRandomR(-0.1, 0.1)

  thetaDD' <- getRandomR (0.001, 0.1) >>= (\a -> uniform [-a, a])
  -- color is determined by direction of movement
  let
    c = round (fromIntegral (length palette - 1) * (theta' ) / twoPi)
    myc' = palette ! c
    mya' = palette ! (length palette - c - 1)


  pure $ Muon x' y' speed' theta' speedD' thetaD' thetaDD' myc' mya'


collideQuark :: Double -> RandGen Quark
collideQuark ct = do
  (w, h) <- getSize
  let
    x' = fromIntegral w / 2
    y' = fromIntegral h / 2
    thetaD' = 0
  speed' <- getRandomR (0.5, 3)
  speedD' <- getRandomR (0.996,1.001)
  theta' <- (+ct) <$> getRandomR (-0.11, 0.11)

  thetaDD' <- getRandomR (0.00001, 0.001) >>= (\a -> uniform [-a, a])

  pure $ Quark x' y' speed' theta' speedD' thetaD' thetaDD' black

collideAxion :: Double -> RandGen Axion
collideAxion ct = do
  (w, h) <- getSize
  let
    x' = fromIntegral w / 2
    y' = fromIntegral h / 2
    thetaD' = 0
  speed' <- getRandomR (1, 6)
  speedD' <- getRandomR (0.998,1.000)
  theta' <- getRandomR (0, twoPi)

  thetaDD' <- getRandomR (0.00001, 0.001) >>= (\a -> uniform [-a, a])

  pure $ Axion x' y' speed' theta' speedD' thetaD' thetaDD'

collideHadron :: Double -> RandGen Hadron
collideHadron ct = do
  (w, h) <- getSize
  let
    x' = fromIntegral w / 2
    y' = fromIntegral h / 2
    thetaD' = 0
    c = hsva 120 1 0.5
  speed' <- getRandomR (0.5, 3.5)
  speedD' <- getRandomR (0.996,1.001)
  theta' <- getRandomR (0, twoPi)

  thetaDD' <- getRandomR (0.00001, 0.001) >>= (\a -> uniform [-a, a])

  pure $ Hadron x' y' speed' theta' speedD' thetaD' thetaDD' c


moveQuark :: Double -> Quark -> RandGen Quark
moveQuark ct m@(Quark x y s th sD thD thDD c) = do
  (w, h) <- getSize
  let
    x' = x + s * sin th
    y' = y + s * cos th
  m3 <- getRandomR (0::Int, 1000)
  if m3 < 3 then pure $ Quark x' y' (s * (-1)) (th + thD) (2 - sD) (thD + thDD) thDD c
  else if x' < fromIntegral (-w) || x' > 2 * fromIntegral w || y' < fromIntegral (-h) || y' > 2 * fromIntegral h
        then collideQuark ct
        else
          pure $ Quark x' y' (s * sD) (th + thD) sD (thD + thDD) thDD c


moveMuon :: Double -> Muon -> RandGen Muon
moveMuon ct m@(Muon x y s th sD thD thDD c a) = do
  (w, h) <- getSize
  let
    x' = x + s * sin th
    y' = y + s * cos th
  if x' < fromIntegral (-w) || x' > 2 * fromIntegral w || y' < fromIntegral (-h) || y' > 2 * fromIntegral h
    then collideMuon ct
    else
      pure $ Muon x' y' (s - sD) (th + thD) sD (thD + thDD) thDD c a

moveAxion :: Double -> Axion -> RandGen Axion
moveAxion ct m@(Axion x y s th sD thD thDD) = do
  (w, h) <- getSize
  let
    x' = x + s * sin th
    y' = y + s * cos th
    newOne = Axion x' y' (s * sD) (th + thD) (sD * 0.9999) (thD + thDD) thDD
  m3 <- getRandomR (0 :: Int, 1000)
  s30 <- getRandomR (0 :: Int, 100)
  if m3 < 4 then
    if s30 < 30 then
      pure $ Axion x' y' (s * (-1)) (th + thD) (2 - sD) (thD + thDD) thDD
    else collideAxion ct
  else pure newOne

moveHadron :: Double -> Hadron -> RandGen Hadron
moveHadron ct m@(Hadron x y s th sD thD thDD c) = do
  (w, h) <- getSize
  let
    x' = x + s * sin th
    y' = y + s * cos th
    newOne = Hadron x' y' (s * sD) (th + thD) (sD * 0.9999) (thD + thDD) thDD c
  m3 <- getRandomR (0 :: Int, 1000)
  s30 <- getRandomR (0 :: Int, 100)

  if m3 < 3
    then if s30 < 30 then collideHadron ct else pure $ Hadron x' y' (s * sD) (th + thD) 1 (thD + thDD) 0.00001 c
    else if x' < fromIntegral (-w) || x' > 2 * fromIntegral w || y' < fromIntegral (-h) || y' > 2 * fromIntegral h
          then collideHadron ct
          else
            pure newOne

main :: IO ()
main = UI.main "BubbleChamber" (World 1200 700 1) renderSetup initialModel step render


renderSetup = do
  (w,h) <- getSize
  pure $ do
    rectangle 0 0 (fromIntegral w ) (fromIntegral h )
    white 1 *> fill

step (Universe ct ms as hs qs) = do
  ms' <- traverse (moveMuon ct) ms
  as' <- traverse (moveAxion ct) as
  hs' <- traverse (moveHadron ct) hs
  qs' <- traverse (moveQuark ct) qs
  pure $ Universe ct ms' as' hs' qs'

render (Universe ct ms as hs qs) = do
  rend <- traverse renderMuon ms
  axs <- traverse renderAxion as
  has <- traverse renderHadron hs
  qus <- traverse renderQuark qs
  let rendering = do
        sequence axs
        sequence rend
        sequence has
        sequence qus
  pure $ rendering

renderHadron :: Hadron -> RandGen (Render ())
renderHadron (Hadron x y _ _ _ _ _ _) =
  pure $ do
    point white 0.11 x (y - 1)
    point black 0.11 x (y + 1)

renderQuark :: Quark -> RandGen (Render ())
renderQuark (Quark x y _ _ _ _ _ c) = do
  (w, _) <- getSize
  pure $ do
    point c 0.125 x y
    point c 0.125 (fromIntegral w - x) y


renderMuon :: Muon -> RandGen (Render ())
renderMuon (Muon x y _ _ _ _ _ c a) = do
  (w, _) <- getSize
  pure $ do
    point c 0.42 x y
    point a 0.42 (fromIntegral w - x) y

--renderAxion :: Axion -> RandGen (Render ())
renderAxion (Axion x y _ _ _ _ _) = do
  (w, _) <- getSize
  pure $ do
    point (hsva 0 0 16) 0.6 x y
    sequence $ (\dy -> point white ((30 - 6 * dy ) / 255) x (y - dy)) <$> [1..4]
    sequence $ (\dy -> point black ((30 - 6 * dy ) / 255) x (y + dy)) <$> [1..4]

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
