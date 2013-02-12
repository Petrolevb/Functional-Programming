module Replay where

import System.Time(getClockTime, ClockTime(TOD))

-- Types
--newtype Replay q r i = Rep ( [Lift | Ask] -> r -> IO i)
data Replay q r i = EmptyReplay 
                  | LiftIO (IO i) (Trace r) (Replay q r i) 
                  | Ask    q      (Trace r) (Replay q r i)
                  | Final i
data Trace r = Answer r (Trace r) 
             | Result r (Trace r)
             | Nil

type Question = String

-- Operations
instance Monad (Replay q r)
    where
--        return :: i -> Replay q r i
        return i = Final i
--       (>>=) :: Replay q r a -> (a -> Replay q r b) -> Replay q r b
        EmptyReplay     >>= f = undefined
        (LiftIO a b c)  >>= f = undefined
        (Ask a b c)     >>= f = undefined
        (Final i)       >>= f = undefined

--instance Show r => Show (Trace r)
instance Show r => Show (Trace r) where
    show Nil = "Nil"
    show (Answer r trace) = "Answer " ++ show r ++ ", " ++ show trace
    show (Result r trace) = "Result " ++ show r ++ ", " ++ show trace
instance Read r => Read (Trace r)

liftIO :: (Show a, Read a) => IO a -> Replay q r a
liftIO io = undefined
--liftIO io = LiftIO io (Item (Result (print.io)) emptyTrace)

ask  :: q -> Replay q r r
ask question = Ask question emptyTrace EmptyReplay

emptyTrace :: Trace r
emptyTrace = Nil
addAnswer  :: Trace r -> r -> Trace r
addAnswer traces a = Answer a traces

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run (Ask q traceRep next) (Answer a trace) = run next (addAnswer traceRep a)
run (Ask q traceRep next) Nil              = return $ Left (q, traceRep)
run _ _= undefined
--run (LiftIO i) _             = undefined
--run (Ask (q, r)) Nil         = return $ Left (q, (addAnswer Nil r))
--run (Ask (q, r)) (Item a trace) =  undefined
--run (Rep f)  Nil           = undefined
--run (Rep f) (Item i trac)  = undefined


example :: Replay String Int ()
example = do
    t0 <- liftIO getTime
    liftIO (putStrLn "Hello!")
 --   age <- ask "What is your age ? "
{-
    liftIO (putStrLn "Age : " ++ age)
    name <- ask "What's your name ? "
    liftIO (putStrLn ("You, " ++ name ++ ", are " ++ age))
    t1 <- liftIO getTime
    liftIO (putStrLn ("Total time : " ++ show (t1 - t0) ++ " seconds"))
--}
--    return (read age)

getTime :: IO Integer
getTime = do
    TOD secs _ <- getClockTime
    return secs

