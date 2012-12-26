import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Mixer
import System.IO
import System.Exit
import System.Random
import System.Environment
import Control.Monad
import Control.Monad.Error

data Cat = Cat { catPos :: Rect
               }

data Config = Config { videoFlags :: [SurfaceFlag]
                     , width :: Int
                     , height :: Int
                     , musicOn :: Bool
                     , dataSet :: String
                     , justHelp :: Bool
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
                   , screen :: Surface
                   , drawArea :: Rect
                   , background :: Pixel
                   , randGen :: StdGen
                   }

data ResourceSet = ResourceSet { catPaths :: [String]
                               , sparklePaths :: [String]
                               , musicPath :: String
                               }
 deriving Show
             
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
advanceSparkleList st spLst = if newSpkPossi == 1
                              then newSpk : advSpk
                              else advSpk
 where advSpk = filter (spkOnscreen st) $ map (advanceSparkle st) spLst
       g = randGen st
       newSpk = sparkleSpawn g (surfaceRect scr) (surfaceRect catS)
       scr = screen st
       catS = head $ catFrames st
       (newSpkPossi, g2) = randomR (1 :: Int, 3) g

advanceState :: State -> State
advanceState st = st { catFrame = (catFrame st + 1) `mod` maxCatFrame
                     , sparkles = filter (spkOnscreen st) newSparkles
                     , randGen = newRnd
                     }
 where maxCatFrame = length $ catFrames st
       newSparkles = advanceSparkleList st (sparkles st)
       (_ , newRnd) = random (randGen st) :: (Int, StdGen)

applyArg :: (String, String) -> Config -> Config
applyArg (arg, opt) conf
  | any (== arg) ["-h", "--help"] =
     conf { justHelp = True }
  | any (== arg) ["-d", "--data-set"] =
     conf { dataSet = opt }
  | any (== arg) ["-f", "--fullscreen"] =
     conf { videoFlags = Fullscreen : oldFlags }
  | any (== arg) ["-F", "--windowed", "--no-fullscreen"] =
     conf { videoFlags = filter fF oldFlags }
  | any (== arg) ["-m", "--music"] =
     conf { musicOn = True }
  | any (== arg) ["-M", "--no-music", "--mute"] =
     conf { musicOn = False }
  | any (== arg) ["-s", "--hw-surface"] =
     conf { videoFlags = HWSurface : filter fS oldFlags }
  | any (== arg) ["-S", "--sw-surface", "--no-hw-surface"] =
     conf { musicOn = False }
  | any (== arg) ["-x", "--width"] =
     conf { width = read opt }
  | any (== arg) ["-y", "--height"] =
     conf { height = read opt }
  | otherwise = conf
 where fS = (\f -> f /= HWSurface || f /= SWSurface)
       fF = (/= Fullscreen)
       oldFlags = videoFlags conf

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src (Just clip) dst (Just offset)
 where offset@(Rect ox oy ow oh) = overlap (Rect x y w h) (surfaceRect dst)
       clip = Rect (abs $ min x 0) (abs $ min y 0) ow oh
       (Rect _ _ w h) = surfaceRect src

argsAndOpts :: [String] -> [(String, String)]
argsAndOpts args = filter (\(a, b) -> head a == '-') $ zip args optSet
 where optSet = (tail args) ++ [""]

catSpawn :: Rect -> Rect -> Cat
catSpawn (Rect _ _ scrW scrH) (Rect _ _ catW catH) = Cat cRect
 where cRect = Rect cX cY catW catH
       cX = scrW `div` 2 - catW `div` 2
       cY = scrH `div` 2 - catH `div` 2

defaultConfig :: Config
defaultConfig = Config { videoFlags = [HWSurface, Fullscreen]
                       , width  = 0
                       , height = 0
                       , musicOn = True
                       , dataSet = "default"
                       , justHelp = False
                       }

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

fileExist :: FilePath -> IO Bool
fileExist p = do
  file <- openFile p ReadMode
  hClose file
  return True
  `catchError` do return $ return False

findResources :: String -> IO ResourceSet
findResources set = do
  installed <- fileExist $ head (catPaths rsInstalled)
  local <- fileExist $ head (catPaths rsLocal)
  if installed
  then return rsInstalled
  else return rsLocal
 where catFiles = map (\x-> "fg0" ++ show x ++ ".png") [0..4]
       sparkleFiles = map (\x-> "bg0" ++ show x ++ ".png") [0..4]
       musicFile = "music.ogg"
       installedPrefix = "/usr/share/nyancat/" ++ set ++ "/"
       localPrefix = "res/" ++ set ++ "/"
       rsInstalled = setFromPrefix installedPrefix
       rsLocal = setFromPrefix localPrefix
       setFromPrefix str =
         ResourceSet { catPaths = map (\s -> str ++ s) catFiles
                     , sparklePaths = map (\s -> str ++ s) sparkleFiles
                     , musicPath = str ++ musicFile
                     }

overlap :: Rect -> Rect -> Rect
overlap r1@(Rect r1x r1y r1w r1h) r2@(Rect r2x r2y r2w r2h)
  | r1x > r2x + r2w = Rect 0 0 0 0
  | r2x > r1x + r1w = Rect 0 0 0 0
  | r1y > r2y + r2h = Rect 0 0 0 0
  | r2y > r1y + r1h = Rect 0 0 0 0
  | otherwise = Rect x y w h
     where x = max r1x r2x
           y = max r1y r2y
           w = if r1x > r2x then r2w - r1x else r1w - r2x
           h = if r1y > r2y then r2h - r1y else r1h - r2y

sparkleSpawn :: StdGen -> Rect -> Rect -> Sparkle
sparkleSpawn g (Rect _ _ scrW scrH) (Rect _ _ spkW spkH) = sp
 where sp = Sparkle { sprkPos = sRect
                    , frame = 0
                    , velocity = startSpd
                    }
       minY = negate spkH
       maxY = scrH
       sRect = Rect sX sY spkW spkH
       sX = scrW - 1
       sY = startY
       (startY, g2) = randomR (minY, maxY) g
       (startSpd, g3) = randomR (10, 40) g2

spkOnscreen :: State -> Sparkle -> Bool
spkOnscreen st sp = not $ spX + spW < 0 || spY + spH < 0 ||
                          spX > arW || spY > arH
 where (Rect arX arY arW arH) = drawArea st
       (Rect spX spY spW spH) = sprkPos sp

surfaceRect :: Surface -> Rect
surfaceRect surf = Rect 0 0 w h
 where w = surfaceGetWidth surf
       h = surfaceGetHeight surf

mainLoop :: State -> IO ()
mainLoop st = do
  mapM_ (drawSparkle st) sparkleLst
  mapM_ (drawCat st) catLst
  Graphics.UI.SDL.flip scr
  mapM_ (\r -> fillRect scr (Just r) bg) blankLst
  delay 70
  quit <- do
    event <- pollEvent
    case event of
      NoEvent -> return False
      Quit -> return True
      KeyDown _ -> return True
      MouseMotion {} -> return True
      _ -> return False
  unless quit $ mainLoop newSt
 where newSt = advanceState st
       catLst = cats st
       sparkleLst = sparkles st
       scr = screen st
       blankLst = map sprkPos sparkleLst ++ map catPos catLst
       bg = background st

usageText :: String
usageText = concat
  [ "\nPlease see the readme file or manpage for proper usage inctructions."
  , "\n:: man dupe"
  , "\n:: cat README\n"
  ]

main :: IO ()
main = withInit [InitEverything] $ do
  args <- getArgs
  let config = foldr applyArg defaultConfig $ argsAndOpts args

  if justHelp config then do
    putStrLn usageText
    exitSuccess
  else return ()

  scr <- if (width config /= 0 && height config /= 0)
            || any (== Fullscreen) (videoFlags config)
    then setVideoMode (width config) (height config) 32 $ videoFlags config
    else setVideoMode 800 600 32 $ videoFlags config
  let fmt = surfaceGetPixelFormat scr
  let scrArea = surfaceRect scr
  setCaption "nyan! nyan! nyan! nyan!" []
  bgColour <- mapRGB fmt 0x00 0x33 0x66
  resources <- findResources $ dataSet config
  catFr <- mapM load $ catPaths resources
  spkFr <- mapM load $ sparklePaths resources
  let catArea = surfaceRect $ head catFr
  let spkArea = surfaceRect $ head spkFr
  rand <- getStdGen

  music <- do loadMUS $ musicPath resources
  if musicOn config then do
    openAudio 22050 AudioS16Sys 2 4096
    playMusic music 0
  else return ()

  fillRect scr (Just scrArea) bgColour
  clearEvents
  mainLoop State { screen = scr
                 , drawArea = surfaceRect scr
                 , catFrames = catFr
                 , sparkleFrames = spkFr
                 , cats = [catSpawn scrArea catArea]
                 , catFrame = 0
                 , sparkles = []
                 , background = bgColour
                 , randGen = rand
                 }

  if musicOn config then do
    haltMusic
    closeAudio
  else return ()
  freeMusic music

 where clearEvents = do
         event <- pollEvent
         case event of
           NoEvent -> return ()
           _ -> clearEvents
