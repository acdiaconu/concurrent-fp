import CCExc

test = (print =<<) . runCC $
  incr 100 . pushPrompt p2R $
  incr 2 . pushPrompt p2L $ 
  incr 200 . shiftP p2L $ \tk -> shiftP p2R $ \lk -> return 3
  where
    incr :: Monad m => Int -> m Int -> m Int
    incr n = ((return . (n +)) =<<)


+ 100 | +2 