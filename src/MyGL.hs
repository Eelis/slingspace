{-# LANGUAGE UnicodeSyntax #-}

module MyGL where

import Graphics.UI.GLUT

rotateRadians :: (Floating c, MatrixComponent c) ⇒ c → Vector3 c → IO ()
rotateRadians r = rotate (r / pi * 180)

red, green, blue, black, white :: Color4 GLclampf
red = Color4 1 0 0 1
green = Color4 0 1 0 1
blue = Color4 0 0 1 1
black = Color4 0 0 0 1
white = Color4 1 1 1 1

glDouble :: Double → GLdouble
glFloat :: Float → GLfloat
glDouble = realToFrac
glFloat = realToFrac
unGLdouble :: GLdouble → Double
unGLfloat :: GLfloat → Float
unGLdouble = realToFrac
unGLfloat = realToFrac
-- Todo: These are horrible.
