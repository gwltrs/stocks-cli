-- Either-centric error handling and IO chaining.
-- General guidelines for interpreting operator names:
-- - Operator name starts with ">>": first argument is an either.
-- - Operator name starts with ">>>": first argument is an either in a monad.
-- - Operator name ends with "|": function produces an either in a monad.
-- - Operator name ends with ".": function produces the final value; ends the chain.
-- - Operator name contains "^": function "lifts" a type into the monad-either convention.
-- - Operator name contains "?": function provides interoperability with maybe.

module Railway where

import Data.Bifunctor (Bifunctor, first)
import Control.Exception (handle, SomeException(..), displayException)
import Data.Functor ((<&>))
import Control.Category ((>>>))
import Data.Either.Extra (maybeToEither)
import Data.Function ((&))

-- Converts an exception-throwing IO to one that now returns a Left 
-- containing the description of the exception it would have thrown 
-- before conversion. Returns intended value in a Right if operation is successful.
toEitherIO :: IO a -> IO (Either String a)
toEitherIO unsafeIO = unsafeIO
    <&> Right
    & handle (\e -> (e :: SomeException) & displayException & Left & pure)

-- Asserts a statement in a chain.
ifTrue :: Bool -> e -> Either e ()
ifTrue b e = if b then Left e else Right ()

-- Lifts a maybe nested in a monad to an either nested in a monad. 
infixl 1 ?^|
(?^|) :: Monad m => m (Maybe b) -> e -> m (Either e b)
(?^|) m e = m <&> maybeToEither e
{-# INLINE (?^|) #-}

-- Binds an either in a monad to an either in a monad.
infixl 1 >>>=|
(>>>=|) :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
(>>>=|) m f = do
    e <- m
    case e of
        Left l -> pure $ Left $ l
        Right r -> f r
{-# INLINE (>>>=|) #-}

-- Binds an either to an either in a monad.
infixl 1 >>=|
(>>=|) :: Monad m => Either e a -> (a -> m (Either e b)) -> m (Either e b)
(>>=|) e f =
    case e of
        Left l -> pure $ Left $ l
        Right r -> f r
{-# INLINE (>>=|) #-}

-- Performs a bind-like operation with a maybe-producing function.
infixl 1 >>?|
(>>?|) :: Monad m => Either e a -> ((a -> Maybe b), e) -> m (Either e b)
(>>?|) ethr (f, err) = 
    case ethr of
        Left l -> pure $ Left $ l
        Right r -> pure $ maybeToEither err $ f r
{-# INLINE (>>?|) #-}

-- Performs a bind-like operation with a maybe-producing function.
infixl 1 >>>?|
(>>>?|) :: Monad m => m (Either e a) -> ((a -> Maybe b), e) -> m (Either e b)
(>>>?|) m (f, err) = do
    ethr <- m
    case ethr of
        Left l -> pure $ Left $ l
        Right r -> pure $ maybeToEither err $ f r
{-# INLINE (>>>?|) #-}

-- Continues the chain while ignoring the previous right value.
-- The name of this operator is an allusion to the monadic >> operator.
infixl 1 >>|
(>>|) :: Monad m => Either e a -> m (Either e b) -> m (Either e b)
(>>|) e b =
    case e of
        Left l -> pure $ Left $ l
        Right _ -> b
{-# INLINE (>>|) #-}

-- Continues the chain while ignoring the previous monad's right value.
-- The name of this operator is an allusion to the monadic >> operator.
infixl 1 >>>|
(>>>|) :: Monad m => m (Either e a) -> m (Either e b) -> m (Either e b)
(>>>|) m b = do
    e <- m
    case e of
        Left l -> pure $ Left $ l
        Right _ -> b
{-# INLINE (>>>|) #-}

-- Continues the chain while ignoring the previous right value.
-- The name of this operator is an allusion to the monadic >> operator.
infixl 1 >>^|
(>>^|) :: Monad m => Either e a -> m b -> m (Either e b)
(>>^|) e b =
    case e of
        Left l -> pure $ Left $ l
        Right _ -> b <&> Right
{-# INLINE (>>^|) #-}

-- Continues the chain while ignoring the previous monad's right value.
-- The name of this operator is an allusion to the monadic >> operator.
infixl 1 >>>^|
(>>>^|) :: Monad m => m (Either e a) -> m b -> m (Either e b)
(>>>^|) m b = do
    e <- m
    case e of
        Left l -> pure $ Left $ l
        Right _ -> b <&> Right
{-# INLINE (>>>^|) #-}

-- Combination of <&> and Prelude.either.
-- Useful for ending railway-style chains.
infixl 1 >>>.
(>>>.) :: Monad m => m (Either a b) -> (a -> c, b -> c) -> m (c)
(>>>.) m (l, r) = m <&> either l r
{-# INLINE (>>>.) #-}

-- Combination of >>= and Prelude.either.
-- Useful for ending railway-style chains.
infixl 1 >>>=.
(>>>=.) :: Monad m => m (Either a b) -> (a -> m c, b -> m c) -> m c
(>>>=.) m (l, r) = m >>= either l r
{-# INLINE (>>>=.) #-}

-- Maps a doubly-nested functor.
infixl 1 <<&>>
(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) f map = f <&> (<&> map)
{-# INLINE (<<&>>) #-}

-- Maps the left side of a bifunctor nested in a functor.
infixl 1 <<&>
(<<&>) :: (Functor f, Bifunctor g) => f (g a b) -> (a -> c) -> f (g c b)
(<<&>) f lMap = f <&> (first lMap)
{-# INLINE (<<&>) #-}