-- Types
Question
Answer
Trace
Replay a

-- Operations
instance Monad Replay
liftIO :: (Show a, Read a) => IO a -> Replay a
ask  :: Question -> Replay Answer

emptyTrace :: Trace
addAnswer :: Trace -> Answer -> Trace

run :: Replay a -> Trace -> IO (Either (Question, Trace) a)