newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f a = Identity (f $ runIdentity a)

instance Applicative Identity where
  pure = Identity
  (<*>) f a = pure $ f' (runIdentity a) where
    f' = runIdentity f

instance Monad Identity where
  return = pure
  (>>=) m f = f $ runIdentity m

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                        case maybe_value of
                              Nothing    -> return Nothing
                              Just value -> runMaybeT $ f value

newtype IdentityT m a = IdentityT { runIdentityT :: m (Identity a) }

instance Monad m => Monad (IdentityT m) where
  return  = IdentityT . return . Identity
  m >>= f = IdentityT $ do a' <- (runIdentityT m)
                           runIdentityT $ f (runIdentity a')
