-- ExceptT-centric error handling and IO chaining.

module Railway where

import Data.Bifunctor (Bifunctor, first, second)
import Control.Exception (handle, SomeException(..), displayException)
import Data.Functor ((<&>))
import Control.Category ((>>>))
import Data.Either.Extra (maybeToEither)
import Data.Function ((&))
import Control.Applicative (liftA2, (<*>))

-- | Obtained by composing an arbitrary monad with the 'Either' monad.
-- Computations are actions that may produce a value or exit with an error.
newtype Railway m e v = Railway { runRailway :: m (Either e v) }




-- -- Asserts a statement in a chain.
-- ifTrue :: Bool -> e -> Either e ()
-- ifTrue b e = if b then Left e else Right ()

-- -- Lifts a maybe nested in a monad to an either nested in a monad. 
-- infixl 1 ?^|
-- (?^|) :: Monad m => m (Maybe b) -> e -> m (Either e b)
-- (?^|) m e = m <&> maybeToEither e
-- {-# INLINE (?^|) #-}

-- -- Binds an either in a monad to an either in a monad.
-- infixl 1 >>>=|
-- (>>>=|) :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
-- (>>>=|) m f = do
--     e <- m
--     case e of
--         Left l -> pure $ Left $ l
--         Right r -> f r
-- {-# INLINE (>>>=|) #-}

-- -- Binds an either to an either in a monad.
-- infixl 1 >>=|
-- (>>=|) :: Monad m => Either e a -> (a -> m (Either e b)) -> m (Either e b)
-- (>>=|) e f =
--     case e of
--         Left l -> pure $ Left $ l
--         Right r -> f r
-- {-# INLINE (>>=|) #-}

-- -- Performs a bind-like operation with a maybe-producing function.
-- infixl 1 >>?|
-- (>>?|) :: Monad m => Either e a -> ((a -> Maybe b), e) -> m (Either e b)
-- (>>?|) ethr (f, err) = 
--     case ethr of
--         Left l -> pure $ Left $ l
--         Right r -> pure $ maybeToEither err $ f r
-- {-# INLINE (>>?|) #-}

-- -- Performs a bind-like operation with a maybe-producing function.
-- infixl 1 >>>?|
-- (>>>?|) :: Monad m => m (Either e a) -> ((a -> Maybe b), e) -> m (Either e b)
-- (>>>?|) m (f, err) = do
--     ethr <- m
--     case ethr of
--         Left l -> pure $ Left $ l
--         Right r -> pure $ maybeToEither err $ f r
-- {-# INLINE (>>>?|) #-}

-- -- Continues the chain while ignoring the previous right value.
-- -- The name of this operator is an allusion to the monadic >> operator.
-- infixl 1 >>|
-- (>>|) :: Monad m => Either e a -> m (Either e b) -> m (Either e b)
-- (>>|) e b =
--     case e of
--         Left l -> pure $ Left $ l
--         Right _ -> b
-- {-# INLINE (>>|) #-}

-- -- Continues the chain while ignoring the previous monad's right value.
-- -- The name of this operator is an allusion to the monadic >> operator.
-- infixl 1 >>>|
-- (>>>|) :: Monad m => m (Either e a) -> m (Either e b) -> m (Either e b)
-- (>>>|) m b = do
--     e <- m
--     case e of
--         Left l -> pure $ Left $ l
--         Right _ -> b
-- {-# INLINE (>>>|) #-}

-- -- Continues the chain while ignoring the previous right value.
-- -- The name of this operator is an allusion to the monadic >> operator.
-- infixl 1 >>^|
-- (>>^|) :: Monad m => Either e a -> m b -> m (Either e b)
-- (>>^|) e b =
--     case e of
--         Left l -> pure $ Left $ l
--         Right _ -> b <&> Right
-- {-# INLINE (>>^|) #-}

-- -- Continues the chain while ignoring the previous monad's right value.
-- -- The name of this operator is an allusion to the monadic >> operator.
-- infixl 1 >>>^|
-- (>>>^|) :: Monad m => m (Either e a) -> m b -> m (Either e b)
-- (>>>^|) m b = do
--     e <- m
--     case e of
--         Left l -> pure $ Left $ l
--         Right _ -> b <&> Right
-- {-# INLINE (>>>^|) #-}

-- -- Produces a tuple containing the right values of the two arguments.
-- -- Useful for producing multiple values before operating on them.
-- infixl 1 >>>*|
-- (>>>*|) :: Monad m => m (Either e a) -> m (Either e b) -> m (Either e (a, b))
-- (>>>*|) l r = do
--     le <- l
--     case le of
--         Left lErr -> pure $ Left $ lErr
--         Right lVal -> do
--             re <- r
--             case re of
--                 Left rErr -> pure $ Left $ rErr
--                 Right rVal -> pure $ Right $ (lVal, rVal)
-- {-# INLINE (>>>*|) #-}

-- -- Produces a tuple containing the right values of the two arguments.
-- -- Useful for producing multiple values before operating on them.
-- infixl 1 >>>**|
-- (>>>**|) :: Monad m => m (Either e (a, b)) -> m (Either e c) -> m (Either e (a, b, c))
-- (>>>**|) l r = do
--     le <- l
--     case le of
--         Left lErr -> pure $ Left $ lErr
--         Right (aVal, bVal) -> do
--             re <- r
--             case re of
--                 Left rErr -> pure $ Left $ rErr
--                 Right cVal -> pure $ Right $ (aVal, bVal, cVal)
-- {-# INLINE (>>>**|) #-}

-- -- Combination of <&> and Prelude.either.
-- -- Useful for ending railway-style chains.
-- infixl 1 >>>.
-- (>>>.) :: Monad m => m (Either a b) -> (a -> c, b -> c) -> m (c)
-- (>>>.) m (l, r) = m <&> either l r
-- {-# INLINE (>>>.) #-}

-- -- Combination of >>= and Prelude.either.
-- -- Useful for ending railway-style chains.
-- infixl 1 >>>=.
-- (>>>=.) :: Monad m => m (Either a b) -> (a -> m c, b -> m c) -> m c
-- (>>>=.) m (l, r) = m >>= either l r
-- {-# INLINE (>>>=.) #-}



-- -- Maps the left side of a bifunctor nested in a functor.
-- infixl 1 <<&>
-- (<<&>) :: (Functor f, Bifunctor g) => f (g a b) -> (a -> c) -> f (g c b)
-- (<<&>) f lMap = f <&> (first lMap)
-- {-# INLINE (<<&>) #-}

-- -- Maps the right side of a bifunctor nested in a functor.
-- infixl 1 <&>>
-- (<&>>) :: (Functor f, Bifunctor g) => f (g a b) -> (b -> c) -> f (g a c)
-- (<&>>) f lMap = f <&> (second lMap)
-- {-# INLINE (<&>>) #-}

-- -- Maps a doubly-nested functor.
-- infixl 1 <<$>>
-- (<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
-- (<<$>>) map f = f <&> (<&> map)
-- {-# INLINE (<<$>>) #-}

-- -- Applies a function a doubly-nested applicative.
-- infixl 1 <<*>>
-- (<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
-- (<<*>>) = liftA2 (<*>)
-- {-# INLINE (<<*>>) #-}