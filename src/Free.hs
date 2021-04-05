{-# LANGUAGE  RankNTypes #-}

module Free (Free(..), liftF, foldFree) where

data Free f a = Free (f (Free f a))
              | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free x) = Free (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> Pure x = Pure (f x)
  Pure f <*> Free x = Free (fmap (fmap f) x)
  Free f <*> x = Free (fmap (<*> x) f)

instance Functor f => Monad (Free f) where
  return = Pure
  Pure x >>= f = f x
  Free x >>= f = Free (fmap (>>= f) x)

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap return

foldFree :: Monad m => (forall r. f r -> m r) -> Free f a -> m a
foldFree _         (Pure x) = return x
foldFree interpret (Free x) = do x' <- interpret x
                                 foldFree interpret x'
                      -- or = interpret x >>= foldFree interpret
