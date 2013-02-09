module Replay where

import System.Time(getClockTime, ClockTime(TOD))

-- Types
newtype Replay q r i = Rep ( q -> r -> i)
data Trace r = Nil | Item r (Trace r)
data Item r = Answer r | Result String
    deriving (Show, Read)

type Question = String

-- Operations
instance Monad (Replay q r)
    where
--        return :: i -> Replay q r i
        return i = Rep (\q -> \r -> i)
--       (>>=) :: Replay q r i1 -> (i1 -> Replay q r i2) -> Replay q r i2
        (>>=)  = undefined
instance Show r => Show (Trace r)
instance Read r => Read (Trace r)

liftIO :: (Show a, Read a) => IO a -> Replay q r a
liftIO = undefined
ask  :: q -> Replay q r r
ask = undefined
--ask question = do
--    putStrLn question
--    answer <- getLine
--    return (Replay question answer answer)

emptyTrace :: Trace r
emptyTrace = Nil
addAnswer  :: Trace r -> r -> Trace r
addAnswer traces a =  Item a traces

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run = undefined

{-
example :: TReplay Int
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

