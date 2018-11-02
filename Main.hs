import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State
import System.Random

import Debug.Trace

import Data.List (transpose)

type Field = [[Cell]]

data Cell = Active   !Int
          | Inactive !Int
          | Void

instance Show Cell where
  show (Active i)   = "A" ++ show i
  show (Inactive i) = "I"++show i
  show Void         = "__"

data Move = MoveLeft | MoveRight | MoveUp | MoveDown 

initField :: [[Cell]]
initField = replicate 4 (replicate 4 Void)

colLine :: [Cell] -> [Cell]
colLine xs = map activate $ iterate f xs !! 4
  where
    activate (Inactive n) = Active n
    activate x            = x 
    f []  = []
    f [x] = [x]
    f (x:xs) = let (y:ys) = f xs
               in case (x,y) of
                   (Active n, Active m) | n == m ->
                        Void : Inactive (n+m) : ys
                   (_       , Void)              ->
                        Void : x              : ys
                   _ ->
                        x : y : ys

-- f (x:xs) = go x xs
--     go x [] = [x]
--     go (Active n) (Active m:ys) | n == m    = Void : go (Inactive (n+m)) ys
--     go x          (Void    :ys)             = Void : go x ys
--     go x          (y       :ys)             = x    : go y ys

collapse :: Move -> [[Cell]] -> [[Cell]]
collapse MoveRight = map colLine 
collapse MoveLeft  = map (reverse . colLine . reverse) 
collapse MoveUp    = transpose . collapse MoveLeft  . transpose
collapse MoveDown  = transpose . collapse MoveRight . transpose

voidCells :: Field -> [(Int,Int)]
voidCells field = fst $
  foldl (\(rs,i) line ->
         let (rs',_) = foldl (\(rs,j) c ->
                               case c of
                                Void -> ((i,j):rs, j+1)
                                _    -> (rs, j+1)) (rs,0) line
         in (rs',i+1)) ([], 0) field 

randomRS :: (MonadState s m, Random a, RandomGen s) => (a, a) -> m a
randomRS r = do
  g <- get
  let (a,g') = randomR r g
  put g'
  return a 
   
modifyList :: [a] -> Int -> (a -> a) -> [a]  
modifyList (x:xs) 0 f = f x:xs
modifyList (x:xs) n f = x:modifyList xs (n-1) f 

randomPop :: RandomGen g => Field -> g -> (Field, g)
randomPop field = runState $ 
  case voidCells field of
   [] -> return field
   ps -> do
     ix <- randomRS (0, length ps - 1)
     let (i,j) = ps !! ix
     n  <- randomRS (0, 3) >>= (return . ([2,2,2,4] !!))
     return $ modifyList field i (\line -> modifyList line j (const $ Active n))
           
renderField :: (Field, g) -> Picture
renderField (field, _) = pictures $ do 
  i <- [0..3]
  j <- [0..3]
  let c = field !! i !! j
  case c of
   Void       -> return $ blank
   Active   n -> return $ draw i j n
   Inactive n -> return $ draw i j n
   where
     draw i j n =
       translate (100*(fromIntegral j-1.5)) (100*(1.5-fromIntegral i)) $
       pictures [color (v2c n) $ rectangleSolid 100 100,
                 color white   $ t n $ scale 0.3 0.3 $ text (show n)]
     t n = translate (-16*m) (-16)
       where m = fromIntegral $ floor $ log (fromIntegral n) / log 10 + eps
             eps = 0.0000001 
     v2c 2  = makeColor 1 0.8 0.8 1
     v2c 4  = makeColor 1 0.6 0.6 1
     v2c 8  = makeColor 1 0.4 0.4 1
     v2c 16 = makeColor 0.9 0.3 0.3 1
     v2c n  = makeColor (0.3 + 0.6 * 4 / m) 0.2 0.2 1
       where m = log (fromIntegral n) / log 2

update :: RandomGen g => (Field, g) -> Move -> (Field, g)
update (field, g) dir =
  let field'        = collapse dir field 
      (field'', g') = randomPop field' g
  in (field'', g')

handleEvent :: RandomGen g => Event -> (Field, g) -> (Field, g)
handleEvent (EventKey (SpecialKey s) Down _ _) (field, g) =
  case s of
   KeyUp    -> update (field, g) MoveUp
   KeyDown  -> update (field, g) MoveDown
   KeyLeft  -> update (field, g) MoveLeft
   KeyRight -> update (field, g) MoveRight
   _        -> (field, g)
handleEvent _ x = x   


putInitCells :: RandomGen g => Field -> g -> (Field, g)
putInitCells f0 g0 =
  iterate (uncurry randomPop) (f0,g0) !! 3 
  
main :: IO () 
main = do
  g <- getStdGen
  play (InWindow "2048" (500,500) (80,80))       
       black
       30
       (putInitCells initField g)
       renderField
       handleEvent
       (const id)
            






            
