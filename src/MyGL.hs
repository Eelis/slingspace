{-# LANGUAGE UnicodeSyntax #-}

module MyGL where

import Graphics.UI.GLUT (rotate, Vector3, Color4(..), GLclampf, MatrixComponent)

rotateRadians :: (Floating c, MatrixComponent c) ⇒ c → Vector3 c → IO ()
rotateRadians r = rotate (r / pi * 180)

red, green, blue, black, white :: Color4 GLclampf
red = Color4 1 0 0 1
green = Color4 0 1 0 1
blue = Color4 0 0 1 1
black = Color4 0 0 0 1
white = Color4 1 1 1 1
