import           Control.Monad.Trans.State.Lazy
import           Data.Functor.Identity

get' :: State s s
get' = state $ \s -> (s,s)

put' :: s -> State s ()
put' s = state $ \_ -> ((),s)

exec :: State s a -> s -> s
exec (StateT sa) s0 =  snd . runIdentity $ sa s0

eval :: State s a -> s -> a
eval (StateT sa) s0 = fst . runIdentity $ sa s0

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)
