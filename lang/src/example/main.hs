import CCExc

uc = (print =<<) . runCC $
  pushPrompt p2R $
  incr 100 . incr 2 . shiftP p2R $ (\tk -> decr 5 $ (return 3))
  
sdc = (print =<<) . runCC $
  incr 100 . pushPrompt p2R $
  incr 2 . shiftP p2R $ \lk -> return 3

ddc = (print =<<) . runCC $
  incr 100 . pushPrompt p2R $
  incr 2 . pushPrompt p2L $ 
  incr 200 . shiftP p2L $ \tk -> shiftP p2R $ \lk -> return 3

incr :: Monad m => Int -> m Int -> m Int
incr n = ((return . (n +)) =<<)

decr :: Monad m => Int -> m Int -> m Int
decr n = ((return . (n -)) =<<)