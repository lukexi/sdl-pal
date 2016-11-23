{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL.Pal
import Graphics.GL

main :: IO ()
main = do
	window <- createGLWindow "SDL-Pal"

	whileWindow window $ \events -> do
		glClearColor 1 1 1 1
		glClear GL_COLOR_BUFFER_BIT
		glSwapWindow window
