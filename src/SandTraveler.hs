{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad.Random
import qualified Data.Array               as A

import           Generate
import           Graphics.Rendering.Cairo
import           Prelude                  hiding (id)
import qualified UI

data City = City
  { x      :: Double
  , y      :: Double
  , friend :: Integer
  , vx     :: Double
  , vy     :: Double
  , idx    :: Integer
  , sands  :: [SandPainter]
  , color  :: ColorFn
  }


data SandPainter = SandPainter
  { p :: Double
  , c :: ColorFn
  , g :: Double
  }

type Model = A.Array Integer City

numCities = 200

main = UI.main "SandTraveler" (World 700 700 1) (UI.basic white) initialModel step render

initialModel :: RandGen Model
initialModel = do
  let
    vt = 4.2 :: Double
    vvt = 0.2
    num = fromIntegral numCities
  ot <- getRandomR (0, twoPi)
  (w,h) <- getSize
  let
    mkCity :: Integer -> RandGen City
    mkCity idx = do
      ss <- sequence [mkSP, mkSP, mkSP]
      rf <- getRandomR (0, numCities `div` 5)
      let
        t = fromIntegral idx
        tinc = ot + (1.1 - t / num) * t * 2 * twoPi / num
        vx' = vt * sin tinc
        vy' = vt * cos tinc
        x' = (fromIntegral w)/2+ 2.0*vx'
        y' = (fromIntegral h)/2+ 2.0*vy'
        friend = (idx + rf ) `mod` numCities
        c = City x' y' friend vx' vy' idx ss white

      pure c

  let cc = A.listArray (0, numCities) (mkCity <$> [0..numCities])
  sequence cc



step :: Model -> RandGen Model
step = pure

render _ = pure $ white 1

distance :: City -> City -> Double
distance (City x y _ _ _ _ _ _) (City x' y' _ _ _ _ _ _) =
  sqrt ((x-x')*(x-x') + (y-y')*(y-y'))

--moveCity :: Model -> City -> City
move m (City{..}) =
  let
    (City fx fy _ _ _ _ _ _) = m A.! friend
    vx' = ((vx + (fx - x) / 1000) * 0.936)
    vy' = ((vy + (fy - y) / 1000) * 0.936)
  in
    City (x + vx') (y + vy') friend vx' vy' idx sands color

mkSP :: RandGen SandPainter
mkSP = do
  p <- getRandomR (0, 1)
  c <- someColorP palette
  g <- getRandomR(0.01, 0.1)
  pure $ SandPainter p c g




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
  , hsva 0.000 0.000 0.000
  , hsva 0.000 0.000 0.000
  , hsva 0.000 0.000 0.000
  , hsva 0.000 0.000 0.000
  , hsva 0.000 0.000 0.000
  , hsva 0.000 0.000 1.000
  , hsva 0.000 0.000 1.000
  , hsva 0.000 0.000 1.000
  , hsva 0.000 0.000 1.000
  , hsva 0.000 0.000 1.000
  , hsva 0.000 0.000 0.000
  , hsva 0.000 0.000 0.000
  , hsva 0.000 0.000 0.000
  , hsva 0.000 0.000 0.000
  , hsva 0.000 0.000 0.000
  , hsva 0.000 0.000 1.000
  , hsva 0.000 0.000 1.000
  , hsva 0.000 0.000 1.000
  , hsva 0.000 0.000 1.000
  , hsva 0.000 0.000 1.000
  ]
