module Replay where

import System.Time(getClockTime, ClockTime(TOD))

-- Types
--newtype Replay q r i = Rep ( [Lift | Ask] -> r -> IO i)
data Replay q r i = EmptyReplay 
                  | LiftIO (IO i) (Trace i)
                  | Ask    q      (Trace r)
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
        (LiftIO io Nil)               >>= f = undefined
        (LiftIO io (Result r trace))  >>= f = f r
        (Ask question Nil) >>= f = undefined
        (Ask question (Answer ans trace)) >>= f = undefined

            -- f ans
--        (Final i)       >>= f = undefined

{-
    liftIO getTime >>=
    (\t1 -> liftIO (putStrLn ("Total time : " ++ show (t1 - t0) ++ " seconds"))) 
    
    (\x -> liftIO x) (LiftIO getTime (Result "13434" trace)
    (\x -> liftIO x) (Ask "what is your age" (Answer "24" trace)

    a = LiftIO (putStr "") (Result "()" Nil) EmptyReplay
    b = LiftIO (putStr "Hello") (Result "()" Nil) EmptyReplay
    
    c = LiftIO (putStr "") (Result "()" Result "()" Nil) b

    c = LiftIO (putStr "Hello") (Result "()" Result "()" Nil) a
    LiftIO (putStr "Salut") (Result "salut" Nil) EmptyReplay


-}

instance Show r => Show (Trace r) where
    show Nil = "EndTrace"
    show (Answer r trace) = "Answer " ++ show r ++ ", " ++ show trace
    show (Result r trace) = "Result " ++ show r ++ ", " ++ show trace
instance Read r => Read (Trace r)

liftIO :: (Show a, Read a) => IO a -> Replay q r a
liftIO io = LiftIO io Nil
--liftIO io = LiftIO io (Result (print.io))  

ask  :: q -> Replay q r r
ask question = Ask question emptyTrace 

emptyTrace :: Trace r
emptyTrace = Nil
addAnswer  :: Trace r -> r -> Trace r
addAnswer traces a = Answer a traces

addTrace :: Trace r -> Trace r -> Trace r
addTrace Nil trace           = trace
addTrace (Result r tr) trace = Result r (addTrace tr trace)
addTrace (Answer r tr) trace = Answer r (addTrace tr trace)


run :: Show a => Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
--run (Ask q traceExec) Nil = Left (q, traceExec)
run (Ask q (Answer a traceExec)) (Answer r trace) = undefined
run (Ask q traceExec) _ = return (Left (q, traceExec))
run (LiftIO io traceExec) Nil = undefined 
run (LiftIO io traceExec) traceParam = undefined
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


running :: Show i => Replay String String i -> IO i
running prog = play emptyTrace prog
play t prog = do
    e <- run prog t
    case e of
        Left (q, t') -> do
            putStr ("Question: " ++ q ++ " ")
            r <- getLine
            play (addAnswer t' r) prog
        Right x -> return x


example :: Replay String String Int
example = do
    t0 <- liftIO getTime
    liftIO (putStrLn "Hello!")
    age <- ask "What is your age ? "
    liftIO (putStrLn ("Age : " ++ age))
    name <- ask "What's your name ? "
    liftIO (putStrLn ("You, " ++ name ++ ", are " ++ age))
    t1 <- liftIO getTime
    liftIO (putStrLn ("Total time : " ++ show (t1 - t0) ++ " seconds"))
    return (read age)

getTime :: IO Integer
getTime = do
    TOD secs _ <- getClockTime
    return secs

