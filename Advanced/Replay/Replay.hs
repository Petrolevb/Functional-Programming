module Replay where

import System.Time(getClockTime, ClockTime(TOD))

-- Types
--newtype Replay q r i = Rep ( [Lift | Ask] -> r -> IO i)
data Replay q r i = EmptyReplay 
                  | LiftIO (IO i) (Trace i) (Replay q r i)
                  | Ask    q      (Trace i) (Replay q r i)
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
--        EmptyReplay     >>= f = undefined -- cannot be used as a parameter for f
        (LiftIO io (Result r trace) rep)  >>= f = f r
        (Ask question Nil rep) >>= f = undefined
        (Ask question (Answer ans trace) rep) >>= f = f ans
            -- f ans
--        (Final i)       >>= f = undefined

-- liftIO getTime >>= (\t0 -> liftIO (putStrLn "Hello!"))

{-
    (\x -> liftIO x) (LiftIO getTime (Result "13434" trace) repl
    (\x -> liftIO x) (Ask "what is your age" (Answer "24" trace) repl

    a = LiftIO (putStr "") (Result "()" Nil) EmptyReplay
    b = LiftIO (putStr "Hello") (Result "()" Nil) EmptyReplay
    
    c = LiftIO (putStr "") (Result "()" Result "()" Nil) b

    c = LiftIO (putStr "Hello") (Result "()" Result "()" Nil) a
    LiftIO (putStr "Salut") (Result "salut" Nil) EmptyReplay


-}

--combine :: Replay q r a -> Replay q r b -> Replay q r b
combine (LiftIO io trace emrep)    rep =  LiftIO io (addTrace trace (getTrace rep)) rep
    where getTrace (LiftIO _ trace _) = trace
          getTrace (Ask    _ trace _) = trace
combine (Ask question trace emrep) rep = Ask question (addTrace trace (getTrace rep)) rep
    where getTrace (LiftIO _ trace _) = trace
          getTrace (Ask    _ trace _) = trace

--instance Show r => Show (Trace r)
instance Show r => Show (Trace r) where
    show Nil = "EndTrace"
    show (Answer r trace) = "Answer " ++ show r ++ ", " ++ show trace
    show (Result r trace) = "Result " ++ show r ++ ", " ++ show trace
instance Read r => Read (Trace r)

liftIO :: (Show a, Read a) => IO a -> Replay q r a
liftIO io = undefined
--liftIO io = LiftIO io (Result (print.io)) emptyTrace EmptyReplay

ask  :: q -> Replay q r r
ask question = Ask question emptyTrace EmptyReplay

emptyTrace :: Trace r
emptyTrace = Nil
addAnswer  :: Trace r -> r -> Trace r
addAnswer traces a = Answer a traces

addTrace :: Trace r -> Trace r -> Trace r
addTrace Nil trace           = trace
addTrace (Result r tr) trace = Result r (addTrace tr trace)
addTrace (Answer r tr) trace = Answer r (addTrace tr trace)


run :: Show a => Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
--run (Ask q traceRep next) (Answer a trace) = run next (addAnswer traceRep a)
--run (Ask q traceRep next) Nil              = return $ Left (q, traceRep)
--run (LiftIO io trace next) (Result a traceExec) = undefined
--run (LiftIO io trace next) Nil                  = do 
--    x <- io
--    run next (Result (show x) trace)
--run EmptyReplay trace = return $ Left (, trace)
run (Final i) _ = return $ Right i
    
--run (LiftIO i) _             = undefined
--run (Ask (q, r)) Nil         = return $ Left (q, (addAnswer Nil r))
--run (Ask (q, r)) (Item a trace) =  undefined
--run (Rep f)  Nil           = undefined
--run (Rep f) (Item i trac)  = undefined


example :: Replay String Int Int
example = do
--    t0 <- liftIO getTime
--    liftIO (putStrLn "Hello!")
    ask "What's your age ? " >>= (\a -> return a)
--    age <- ask "What is your age ? "
{-
    liftIO (putStrLn "Age : " ++ age)
    name <- ask "What's your name ? "
    liftIO (putStrLn ("You, " ++ name ++ ", are " ++ age))
    t1 <- liftIO getTime
    liftIO (putStrLn ("Total time : " ++ show (t1 - t0) ++ " seconds"))
--}
--    return age

getTime :: IO Integer
getTime = do
    TOD secs _ <- getClockTime
    return secs

