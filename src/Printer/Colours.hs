{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Printer.Colours where

import           Verset
import qualified Data.Colour.SRGB as SRGB
import qualified System.Console.ANSI as An


-- Fully qualified colour definitions using SRGB.Colour Float
white :: SRGB.Colour Float
white = SRGB.sRGB 1 1 1

black :: SRGB.Colour Float
black = SRGB.sRGB 0 0 0

red :: SRGB.Colour Float
red = SRGB.sRGB 1 0 0

green :: SRGB.Colour Float
green = SRGB.sRGB 0 1 0

blue :: SRGB.Colour Float
blue = SRGB.sRGB 0 0 1

yellow :: SRGB.Colour Float
yellow = SRGB.sRGB 1 1 0

magenta :: SRGB.Colour Float
magenta = SRGB.sRGB 1 0 1

cyan :: SRGB.Colour Float
cyan = SRGB.sRGB 0 1 1

gray :: SRGB.Colour Float
gray = SRGB.sRGB 0.5 0.5 0.5

darkRed :: SRGB.Colour Float
darkRed = SRGB.sRGB (128/255) 0 0


-- Less safe, pretty colours
orange :: SRGB.Colour Float
orange = SRGB.sRGB (255/255) (165/255) 0

pink :: SRGB.Colour Float
pink = SRGB.sRGB (255/255) (105/255) (180/255)

turquoise :: SRGB.Colour Float
turquoise = SRGB.sRGB (64/255) (224/255) (208/255)

violet :: SRGB.Colour Float
violet = SRGB.sRGB (138/255) (43/255) (226/255)

lime :: SRGB.Colour Float
lime = SRGB.sRGB (50/255) (205/255) (50/255)

teal :: SRGB.Colour Float
teal = SRGB.sRGB 0 (128/255) (128/255)

indigo :: SRGB.Colour Float
indigo = SRGB.sRGB (75/255) 0 (130/255)

salmon :: SRGB.Colour Float
salmon = SRGB.sRGB (250/255) (128/255) (114/255)

gold :: SRGB.Colour Float
gold = SRGB.sRGB (255/255) (215/255) 0

skyBlue :: SRGB.Colour Float
skyBlue = SRGB.sRGB (135/255) (206/255) (235/255)

slateGray :: SRGB.Colour Float
slateGray = SRGB.sRGB (112/255) (128/255) (144/255)


crimson :: SRGB.Colour Float
crimson = SRGB.sRGB (220/255) (20/255) (60/255)

forestGreen :: SRGB.Colour Float
forestGreen = SRGB.sRGB (34/255) (139/255) (34/255)

navyBlue :: SRGB.Colour Float
navyBlue = SRGB.sRGB 0 0 (128/255)

orchid :: SRGB.Colour Float
orchid = SRGB.sRGB (218/255) (112/255) (214/255)

coral :: SRGB.Colour Float
coral = SRGB.sRGB (255/255) (127/255) (80/255)

khaki :: SRGB.Colour Float
khaki = SRGB.sRGB (240/255) (230/255) (140/255)

plum :: SRGB.Colour Float
plum = SRGB.sRGB (221/255) (160/255) (221/255)

mediumSlateBlue :: SRGB.Colour Float
mediumSlateBlue = SRGB.sRGB (123/255) (104/255) (238/255)

lightSeaGreen :: SRGB.Colour Float
lightSeaGreen = SRGB.sRGB (32/255) (178/255) (170/255)

darkOliveGreen :: SRGB.Colour Float
darkOliveGreen = SRGB.sRGB (85/255) (107/255) (47/255)

steelBlue :: SRGB.Colour Float
steelBlue = SRGB.sRGB (70/255) (130/255) (180/255)

sienna :: SRGB.Colour Float
sienna = SRGB.sRGB (160/255) (82/255) (45/255)




------------------------------------
--
printAllColours :: IO ()
printAllColours = do
  An.setSGR [An.SetConsoleIntensity An.BoldIntensity]
  putText "All colours:"
  An.setSGR [An.Reset]
  traverse_ (uncurry printColourName) allColours

printColourName :: Text -> SRGB.Colour Float -> IO ()
printColourName name colour = do
  An.setSGR [An.SetRGBColor An.Foreground colour]
  putText $ "   " <> name
  An.setSGR [An.Reset]


allColours :: [(Text, SRGB.Colour Float)]
allColours =
  [ ("white", white)
  , ("black", black)
  , ("red", red)
  , ("green", green)
  , ("blue", blue)
  , ("yellow", yellow)
  , ("magenta", magenta)
  , ("cyan", cyan)
  , ("gray", gray)
  , ("darkRed", darkRed)

  -- Less safe
  , ("orange", orange)
  , ("pink", pink)
  , ("turquoise", turquoise)
  , ("violet", violet)
  , ("lime", lime)
  , ("teal", teal)
  , ("indigo", indigo)
  , ("salmon", salmon)
  , ("gold", gold)
  , ("skyBlue", skyBlue)
  , ("slateGray", slateGray)

  -- Rich extras
  , ("crimson", crimson)
  , ("forestGreen", forestGreen)
  , ("navyBlue", navyBlue)
  , ("orchid", orchid)
  , ("coral", coral)
  , ("khaki", khaki)
  , ("plum", plum)
  , ("mediumSlateBlue", mediumSlateBlue)
  , ("lightSeaGreen", lightSeaGreen)
  , ("darkOliveGreen", darkOliveGreen)
  , ("steelBlue", steelBlue)
  , ("sienna", sienna)
  ]


