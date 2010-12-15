
data Color4 a = Color4 a a a a
data Vector3 a = Vector3 a a a

data Key
  = Char Char
  | SpecialKey SpecialKey
  | MouseButton MouseButton

data SpecialKey
  = KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyLeft
  | KeyUp
  | KeyRight
  | KeyDown
  | KeyPageUp
  | KeyPageDown
  | KeyHome
  | KeyEnd
  | KeyInsert

data MouseButton
  = LeftButton
  | MiddleButton
  | RightButton
  | WheelUp
  | WheelDown

data KeyState
  = Down
  | Up

{-! for Color4 derive : Read !-}
{-! for Vector3 derive : Read !-}
{-! for Key derive : Read !-}
{-! for MouseButton derive : Read !-}
{-! for SpecialKey derive : Read !-}
{-! for KeyState derive : Read !-}

