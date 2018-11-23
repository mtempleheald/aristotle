import Control.Monad
import Data.Time

main = do          
          putStrLn ""
          -- putStrLn :: String -> IO ()
          putStrLn "Read/write from/to file output.txt..."
          -- writeFile :: FilePath -> String -> IO ()
          writeFile "output.txt" "hello world!"
          -- appendFile :: FilePath -> String -> IO ()
          appendFile "output.txt" "\nhello again!"
          -- readFile :: FilePath -> IO String
          readFile "output.txt" >>= putStrLn          


          putStrLn ""
          putStrLn "Output current date/time..."
          -- UTCTime :: Day -> DiffTime -> UTCTime
          -- getCurrentTime :: IO UTCTime
          -- ZonedTime :: LocalTime -> TimeZone -> ZonedTime
          -- getZonedTime :: IO ZonedTime
          -- addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
          -- get local time
          localTime <- getZonedTime
          putStrLn $ "Local:  " ++ show localTime
          -- convert to UTC 
          let utcTime = zonedTimeToUTC localTime
          -- or just get UTC in the first place (preferred)
          utcTime <- getCurrentTime
          putStrLn $ "UTC:    " ++ show utcTime
          -- extract TimeZone so we can convert UTC back to local time 
          -- (only for communicating with users)
          let ZonedTime _ myTimeZone = localTime
          let newLocalTime = utcToZonedTime myTimeZone utcTime
          -- add 45 minutes to our time
          let utcPlus45 = addUTCTime (45*60) utcTime
          -- format time for output
          let oraFormat = formatTime defaultTimeLocale "%d-%b-%y %T" utcTime
          putStrLn $ "Oracle: " ++ oraFormat

{-
(=<<)  :: (a -> m b) -> m a -> m b
mapM  :: (a -> m b) -> [a] -> m [b]
mapM_  :: (a -> m b) -> [a] -> m ()          -- mapM print [..] print list to IO
filterM  :: (a -> m Bool) -> [a] -> m [a]
foldM  :: (a -> b -> m a) -> a -> [b] -> m a 
sequence  :: [m a] -> m [a]                  -- "execute" list of actions
sequence_ :: [m a] -> m ()
liftM  :: (a -> b) -> m a -> m b             -- lift pure function into Monad
when  :: Bool -> m () -> m ()                -- execute monadic action if condition met
join  :: m (m a) -> m a                      -- monadic concat (equivalent on list)
-}

          putStrLn ""
          putStrLn "Sum a list [1..5], outputting progress..."
          -- foldM  :: (a -> b -> m a) -> a -> [b] -> m a
          foldM (\a b ->
                 putStrLn (show a ++ "+" ++ show b ++ "=" ++ show (a+b)) 
                 >>
                 return (a+b)
                ) 0 [1..5]
          

          putStrLn ""
          putStrLn "Output list contents..."
          mapM print ["a","c","e"]
          

          putStrLn ""
          putStrLn "Output file contents with lines numbered..."
          numberFile "output.txt"

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith (+) [1,2,3] [2,4,5] = [3,6,8]
numberFile :: FilePath -> IO ()
numberFile fp = do
  l <- lines `liftM` readFile fp                     -- list of lines from file
  let n = zipWith (\n t -> show n ++ ' ' : t) [1..] l
  mapM_ putStrLn n