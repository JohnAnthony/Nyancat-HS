import Graphics.UI.SDL
import Graphics.UI.SDL.Image

data Cat = Cat { catPos :: Rect
               }

data Sparkle = Sparkle { sprkPos :: Rect
                       , frame :: Int
                       , velocity :: Int
                       }

data State = State { cats :: [Cat]
                   , catFrame :: Int
                   , catFrames :: [Surface]
                   , sparkles :: [Sparkle]
                   , sparkleFrames :: [Surface]
                   , running :: Bool
                   , screen :: Surface
                   , drawArea :: Rect
                   , background :: Pixel
                   }
             
advanceSparkle :: State -> Sparkle -> Sparkle
advanceSparkle st sp = sp { sprkPos = newRect
                          , frame = newFrame
                          }
 where oldRect = sprkPos sp
       newRect = oldRect { rectX = rectX oldRect - velocity sp }
       maxFrame = length $ sparkleFrames st
       oldFrame = frame sp
       newFrame = (oldFrame + 1) `mod` maxFrame

advanceSparkleList :: State -> [Sparkle] -> [Sparkle]
advanceSparkleList st spLst = filter (spkOnscreen st) advSpk
 where advSpk = map (advanceSparkle st) spLst

advanceState :: State -> State
advanceState st = st { catFrame = (catFrame st + 1) `mod` maxCatFrame
                     , sparkles = filter (spkOnscreen st) newSparkles
                     }
 where maxCatFrame = (length $ catFrames st)
       newSparkles = advanceSparkleList st (sparkles st)

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src Nothing dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

catSpawn :: Rect -> Rect -> Cat
catSpawn (Rect _ _ scrW scrH) (Rect _ _ catW catH) = Cat cRect
 where cRect = Rect cX cY catW catH
       cX = scrW `div` 2 - catW `div` 2
       cY = scrH `div` 2 - catH `div` 2

drawCat :: State -> Cat -> IO Bool
drawCat st c = applySurface x y cSurf scr
 where catP = catPos c
       x = rectX catP
       y = rectY catP
       cSurf = catFrames st !! catFrame st
       scr = screen st

drawSparkle :: State -> Sparkle -> IO Bool
drawSparkle st sp = applySurface x y spSurf scr
 where scr = screen st
       spSurf = sparkleFrames st !! frame sp
       spkRect = sprkPos sp
       x = rectX spkRect
       y = rectY spkRect

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormat

sparkleSpawn :: Rect -> Rect -> Sparkle
sparkleSpawn (Rect _ _ scrW scrH) (Rect _ _ spkW spkH) = sp
 where sp = Sparkle { sprkPos = sRect
                    , frame = 0
                    , velocity = 5
                    }          
       sRect = Rect sX sY spkW spkH
       sX = scrW
       sY = 10

spkOnscreen :: State -> Sparkle -> Bool
spkOnscreen st sp = True
 where area = drawArea st
       spRect = sprkPos sp
       (Rect spX spY spW spH) = spRect

surfaceRect :: Surface -> Rect
surfaceRect surf = Rect 0 0 w h
 where w = surfaceGetWidth surf
       h = surfaceGetHeight surf

mainLoop :: State -> IO ()
mainLoop st = do
  mapM (drawSparkle st) sparkleLst
  mapM (drawCat st) catLst
  Graphics.UI.SDL.flip scr
  mapM (\r -> fillRect scr (Just r) bg) blankLst
  delay 50
  mainLoop newSt
 where newSt = advanceState st
       catLst = cats st
       sparkleLst = sparkles st
       scr = screen st
       blankLst = map sprkPos sparkleLst ++ map catPos catLst
       bg = background st

main :: IO ()
main = withInit [InitEverything] $ do
    scr <- setVideoMode 640 480 32 [HWSurface]
    let fmt = surfaceGetPixelFormat scr
    let scrArea = surfaceRect scr
    setCaption "nyan! nyan! nyan! nyan!" []
    bgColour <- mapRGB fmt 0x00 0x33 0x66
    catFr <- mapM loadImage [ "res/default/fg00.png" 
                            , "res/default/fg01.png"
                            , "res/default/fg02.png"
                            , "res/default/fg03.png"
                            , "res/default/fg04.png"
                            ]
    spkFr <- mapM loadImage [ "res/default/bg00.png"
                            , "res/default/bg01.png"
                            , "res/default/bg02.png"
                            , "res/default/bg03.png"
                            , "res/default/bg04.png"
                            ]
    let catArea = surfaceRect (catFr !! 0)
    let spkArea = surfaceRect (spkFr !! 0)
    fillRect scr (Just scrArea) bgColour
    mainLoop $ State { screen = scr
                     , drawArea = surfaceRect scr
                     , catFrames = catFr
                     , sparkleFrames = spkFr
                     , cats = [catSpawn scrArea catArea]
                     , catFrame = 0
                     , sparkles = [sparkleSpawn scrArea spkArea]
                     , background = bgColour
                     , running = True
                     }
