module Replay where

import System.Time(getClockTime, ClockTime(TOD))

-- Types
data Replay q r a
data Trace r
-- Operations
instance Monad (Replay q r)
    where return = undefined
          (>>=)  = undefined
instance Show r => Show (Trace r)
instance Read r => Read (Trace r)

liftIO :: (Show a, Read a) => IO a -> Replay q r a
liftIO = undefined
ask  :: q -> Replay q r r
ask = undefined

emptyTrace :: Trace r
emptyTrace = undefined
addAnswer  :: Trace r -> r -> Trace r
addAnswer = undefined

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run = undefined

{-
example :: Replay Int
example = do
    t0 <- liftIO getTime
    liftIO (putStrLn "Hello!")
    age <- ask "What is your age ? "
    name <- ask "What's your name ? "
    liftIO (putStrLn ("You, " ++ name ++ ", are " ++ age))
    t1 <- liftIO getTime
    liftIO (putStrLn ("Total time : " ++ show (t1 - t0) ++ " seconds"))
    return (read age)
--}
getTime :: IO Integer
getTime = do
    TOD secs _ <- getClockTime
    return secs

