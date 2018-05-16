{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad.Random
import qualified Data.Array               as A

import           Generate
import           Graphics.Rendering.Cairo
import           Prelude                  hiding (id)
import qualified UI

data Orbital = Orbital
  { id    :: Integer
  , pid   :: Integer
  , r     :: Double
  , t     :: Double
  , tv    :: Double
  , tvd   :: Double
  , x     :: Double
  , y     :: Double
  , d     :: Integer
  , color :: ColorFn
  }

type Model = A.Array Integer Orbital

main = UI.main "Orbitals" (World 700 700 1) renderSetup initialModel step render

initialModel :: RandGen Model
initialModel = do
  let zeros = (zeroOrb white) <$> [0..499]
  loop 0 (A.listArray (0,499) zeros)

step :: Model -> Generate.RandGen Model
step m = pure $ (\ o@(Orbital i p _ _ _ _ _ _ _ _) -> orbit o (m A.! p)) <$> m

-- render :: Model -> RandGen (Render ())
render m = do
  rendOrbs <- traverse (renderOrbital m) m
  pure $ sequence rendOrbs


sumtv :: Model -> Orbital -> Double
sumtv m (Orbital {..}) =
  if pid == id
    then tv + 1
    else tv + sumtv m (m A.! pid)


renderOrbital :: Model -> Orbital -> RandGen (Render ())
renderOrbital m orb@(Orbital i p r t tv tvd x' y' d col) = do
  fzx <- getRandomR(-0.22, 0.22)
  fzy <- getRandomR(-0.22, 0.22)
  o <- getRandomR (0, twoPi)
  o' <- getRandomR (0,1)
  pure $ do
    point col 0.165 (x' + fzx) (y' + fzy)
    when (sumtv m orb < 1.00001) $ do
      let
        (Orbital _ _ _ _ _ _ px py _ _) = (m A.! p)

        fzx = r * cos o + px
        fzy = r * sin o + py
        fzx' = x' + o' * (px - x')
        fzy' = y' + o' * (py - y')
      point col 0.07 fzx fzy
      point black 0.07 fzx' fzy'


renderSetup :: RandGen (Render ())
renderSetup = do
  (w,h) <- getSize
  pure $ do
    rectangle 0 0 (fromIntegral w ) (fromIntegral h )
    white 1 *> fill

loop :: Integer -> Model -> RandGen Model
loop idx model = do
  let
    len = toInteger $ length model
    th = round (trasholdP * fromIntegral len)
  (w,h) <- getSize
  npid <- if idx > th then getRandomR (0, idx) else pure idx
  o <- orbital model idx npid
  let newModel =
        if idx == npid
          then model A.// [(idx, setPosition o (fromIntegral  w/2) (fromIntegral h/2))]
          else model A.// [(idx, o)]
  if idx == len - 1 then pure newModel else loop (idx + 1) newModel


setPosition o px py =
  Orbital (id o) (pid o) (r o) (t o) (tv o) (tvd o) px py (d o) (color o)

orbit Orbital {..} (Orbital _ _ _ _ _ _ px py _ _) = Orbital id pid r (t+tv) (tv * 0.99942) tvd (px + r * cos t) (py + r * sin t) d color




orbital :: Model -> Integer -> Integer -> RandGen Orbital
orbital m id pid = do
  (w,h) <- getSize
  c' <- someColorP palette
  let
    d' = d (m A.! pid) + 1
    t' = - (pi / 2)
  r' <- getRandomR(1, 0.4 * fromIntegral w / fromIntegral d')
  tv'' <- getRandomR(0.0001, 0.02 / (fromIntegral d' + 1))
  tv' <- uniform [tv'', -tv'']
  tvd' <- getRandomR(1.001, 1.010)

  if id == pid
    then pure $ Orbital id pid 0 0 0 0 0 0 0  c'
    else pure $ Orbital id pid r' t' tv' tvd' 0 0 d' c'

trasholdP :: Double
trasholdP = 0.1

zeroOrb c pid = Orbital pid pid 0 0 0 0 0 0 0 c

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
