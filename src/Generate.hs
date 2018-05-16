{-# LANGUAGE RecordWildCards #-}

module Generate (module Generate, module Data.GI.Base) where

import           Control.Arrow                     ((&&&))
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Colour.RGBSpace              (RGB (..))
import           Data.Colour.RGBSpace.HSV          (hsv)
import           Data.GI.Base
import           Foreign.Ptr                       (castPtr)
import qualified GI.Cairo
import           Graphics.Rendering.Cairo          (fill, rectangle,
                                                    setSourceRGBA)
import           Graphics.Rendering.Cairo.Internal (Render (runRender))
import           Graphics.Rendering.Cairo.Types    (Cairo (Cairo))



data World = World
  { worldWidth  :: Integer
  , worldHeight :: Integer
  , worldScale  :: Double
  }
type RandGen a = ReaderT World (Rand StdGen) a
type ColorFn = Double -> Render()

someColorP :: [ColorFn] -> RandGen ColorFn
someColorP pal = uniform pal

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))


run :: World -> StdGen -> RandGen a -> (a, StdGen)
run w g =
  flip runRand g
  . flip runReaderT w

getSize :: RandGen (Integer, Integer)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure ( w,  h)



hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
  where RGB{..} = hsv h s v

point :: ColorFn -> Double -> Double -> Double -> Render ()
point c a x y = do
  rectangle (roundD x) (roundD y) 1 1
  c a *> fill

roundD :: Double -> Double
roundD = fromIntegral . round


white :: Double -> Render ()
white = hsva 0 0 1

black :: Double -> Render ()
black = hsva 0 0 0

twoPi = 2.0 * pi :: Double
