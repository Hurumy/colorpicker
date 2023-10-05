{-# LANGUAGE RecordWildCards #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Lib


wWidth, wHeight :: Num a => a
wWidth  = 640
wHeight = 480

cSize, cWidth, cHeight :: Num a => a
cSize = 20
cWidth = fromIntegral $ wWidth `div` cSize
cHeight = fromIntegral $ wHeight `div` cSize

window :: Display
window = InWindow "ColorPicker" (wWidth, wHeight) (100, 100)

main :: IO ()
main = do
	info <- initInfo
	playIO window white 10 info testWindow eventHandler stepInfo

testWindow :: Info -> IO Picture
testWindow i@Info{..} = pure $ pictures[ translate 100 100 $ color (makeColorI _red _green _blue 255) $ circleSolid 50]

data Info = Info
	{ _key :: KeyInput
	, _red :: Int
	, _green :: Int
	, _blue :: Int
	}

initInfo :: IO Info
initInfo = pure $ Info {_key = INone, _red = 200, _green = 200, _blue = 100}


data KeyInput = IUpR | IDwR | IUpG | IDwG | IUpB | IDwB | INone

changeColor :: KeyInput -> Info -> IO Info
changeColor IUpR p@Info{..} = pure $ p{ _red = _red + 1 }
changeColor IDwR p@Info{..} = pure $ p{ _red = _red - 1 }
changeColor IUpG p@Info{..} = pure $ p{ _green = _green + 1 }
changeColor IDwG p@Info{..} = pure $ p{ _green = _green - 1 }
changeColor IUpB p@Info{..} = pure $ p{ _blue = _blue + 1 }
changeColor IDwB p@Info{..} = pure $ p{ _blue = _blue - 1 }
changeColor _ p = pure $ p

{--
pickColor :: PickedColor -> Color
pickColor p@PickedColor{..} = makeColorI _red _green _blue 0

--}

updateWindow :: Info -> IO Picture
updateWindow i@Info{..} = 
	pure $ pictures
	[ translate 100 100 $ color (makeColorI 100 100 100 0) $ circleSolid 50
	--, translate (-wWidth/2+10) (-wHeight/2+10) . scale 0.2 0.2 $ text ("Red: " ++ show _red ++ " Green: " ++ show _green ++ " Blue: " ++ show _blue)
	]

eventHandler :: Event -> Info -> IO Info
eventHandler e i = case e of
	EventKey (Char 'f')		Down _ _ -> pure $ i { _key = IUpR }
	EventKey (Char 'v')		Down _ _ -> pure $ i { _key = IDwR }
	EventKey (Char 'g')		Down _ _ -> pure $ i { _key = IUpG }
	EventKey (Char 'b')		Down _ _ -> pure $ i { _key = IDwG }
	EventKey (Char 'h')		Down _ _ -> pure $ i { _key = IUpB }
	EventKey (Char 'n')		Down _ _ -> pure $ i { _key = IDwB }
	_ -> pure i

stepInfo :: Float -> Info -> IO Info
stepInfo f i@Info{..} = changeColor _key i


{--
stepInfo :: Info -> IO Info
stepInfo i@Info{..} = do
	let newcolor = changeColor _key _color
	pure $ i { _color = newcolor, _key = INone}
--}



