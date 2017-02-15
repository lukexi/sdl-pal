{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL.Pal
import Graphics.GL
import Data.Time

main :: IO ()
main = do
    window <- createGLWindow "SDL-Pal"

    whileWindow window $ \events -> do
        now <- (/2) . (+1) . sin . realToFrac . utctDayTime <$> getCurrentTime
        glClearColor now 0.1 0.2 1
        glClear GL_COLOR_BUFFER_BIT
        glSwapWindow window
