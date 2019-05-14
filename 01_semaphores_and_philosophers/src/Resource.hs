module Resource where
    
type Resource = TVar Int

check :: Bool -> STM ()
check True  = return ()
check False = retry

acquire :: Resource - Int - STM ()
acquire res nr = do n <- readTVar res
                    check ( nr >= n )
                    writeTVar res (n - nr)
                            
release :: Resource - Int - STM ()
release res nr = do n <- readTVar res
                    writeTVar res (n + nr)