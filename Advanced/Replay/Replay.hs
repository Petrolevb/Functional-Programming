-- Types
Replay q r a
Trace r

-- Operations
instance Monad (Replay q r)
instance Show r => Show (Trace r)
instance Read r => Read (Trace r)

liftIO :: (Show a, Read a) => IO a -> Replay q r a
ask  :: q -> Replay q r r

emptyTrace :: Trace r
addAnswer  :: Trace r -> r -> Trace r

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)