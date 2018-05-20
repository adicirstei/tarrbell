{-# LANGUAGE OverloadedLabels #-}
--{-# LANGUAGE PartialTypeSignatures #-}

module UI where

import           Generate

import           Control.Concurrent       (yield)
import           Data.GI.Base
import           Data.IORef
import           Data.Semigroup           ((<>))
import           Data.Time.Clock.POSIX
import qualified GI.GLib                  as GLib
import qualified GI.Gtk                   as Gtk

import           Control.Monad.Random     (evalRandIO, getRandomR, mkStdGen)

import           Graphics.Rendering.Cairo

main :: String -> World -> RandGen (Render a) -> RandGen m -> (m -> RandGen m) -> (m -> RandGen (Render b)) -> IO ()
main name world setup model step renderModel = do
  seed <- round . (*1000) <$> getPOSIXTime
  -- seed <- pure 1526387020654
  let

    wWidth = worldWidth world
    wHeight = worldHeight world
    runWithWorld = run world
    gen = mkStdGen seed
    initialModel' = runWithWorld gen model
  sourface <- createImageSurface FormatARGB32 (fromIntegral wWidth) (fromIntegral wHeight)
  let (r,_) = runWithWorld gen setup
  renderWith sourface r

  modelRef <- newIORef initialModel'
  imageRef <- newIORef sourface
  let stepAct = do
                i <- readIORef imageRef
                (m,g) <- readIORef modelRef
                srf <- readIORef imageRef

                let (m', g') = runWithWorld g $ step m
                let (r, g'') = runWithWorld g' $ renderModel m'
                renderWith srf r

                atomicWriteIORef imageRef srf
                atomicWriteIORef modelRef (m',g')

  let drawCB modelRef imageRef ctx = do

        srf <- readIORef imageRef
        renderWithContext ctx $ do
          setSourceSurface srf 0 0
          paint
          pure ()

        pure  True

  _ <- Gtk.init Nothing

  window <- new Gtk.Window [#defaultWidth := fromIntegral wWidth , #defaultHeight := fromIntegral wHeight ]

  on window #destroy $ saveImage imageRef seed name >> Gtk.mainQuit

  on window #draw (drawCB modelRef imageRef)
  on window #keyPressEvent $ \_ -> do
    saveImage imageRef seed name
    return False

  GLib.timeoutAdd GLib.PRIORITY_DEFAULT 20 (stepAct >> yield >> #queueDraw window >> yield >>  pure True)

  #showAll window

  Gtk.main



saveImage imgRef seed name = do
  r <- evalRandIO $ getRandomR(toInteger  10000, toInteger 99999)
  srf <- readIORef imgRef
  surfaceWriteToPNG srf
    $ "images/" <> name <> "-"
    <> show seed <> "-" <> show r <> ".png"

basic :: ColorFn -> RandGen (Render ())
basic c = do
  (w,h) <- getSize
  pure $ do
    rectangle 0 0 (fromIntegral w ) (fromIntegral h )
    c 1 *> fill
