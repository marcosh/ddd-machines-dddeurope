{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Machines where

import qualified Control.Category as Cat (Category (id, (.)))
import Data.Bifunctor (first)
import Data.Foldable (foldlM)

newtype MealyT m a b = MealyT
  { runMealyT :: a -> m (b, MealyT m a b) }

instance Monad m => Cat.Category (MealyT m) where
  id :: MealyT m a a
  id = stateless id

  (.) :: MealyT m b c -> MealyT m a b -> MealyT m a c
  (.) (MealyT m1) (MealyT m2) = MealyT $ \a -> do
    (b, m2') <- m2 a
    (c, m1') <- m1 b
    pure (c, m1' Cat.. m2')

mealyT :: Functor m => (s -> a -> m (b, s)) -> s -> MealyT m a b
mealyT f = MealyT . (fmap . fmap . fmap $ mealyT f) . f

type Mealy a b = forall m . Monad m => MealyT m a b

mealy :: (s -> a -> (b, s)) -> s -> Mealy a b
mealy = mealyT . (fmap . fmap $ pure)

statefulT :: Functor m => (s -> a -> m s) -> s -> MealyT m a s
statefulT = mealyT . ((fmap (\a -> (a, a)) .) .)

stateful :: (s -> a -> s) -> s -> Mealy a s
stateful = statefulT . (fmap . fmap $ pure)

mooreT :: Functor m => (s -> m (b, a -> s)) -> s -> MealyT m a b
mooreT f = mealyT (\s a -> fmap ($ a) <$> f s)

moore :: (s -> (b, a -> s)) -> s -> Mealy a b
moore = mooreT . (\f s -> pure (($) <$> f s))

statelessT :: Functor m => (a -> m b) -> MealyT m a b
statelessT f = mealyT (\() a -> (, ()) <$> f a) ()

stateless :: (a -> b) -> Mealy a b
stateless = statelessT . (pure .)

{- | Iteratively passes a sequence of arguments to a machine accumulating the results in a Semigroup.
It returns also a new version of the machine with the status updated after all the applications.
-}
run :: (Monad m, Semigroup b, Foldable f) => MealyT m a b -> b -> f a -> m (b, MealyT m a b)
run mealy' initial = foldlM
  (\(b, mealy'') a -> first (b <>) <$> runMealyT mealy'' a)
  (initial, mealy')

compose :: (Monad m, Semigroup c, Foldable f) => c -> MealyT m b c -> MealyT m a (f b) -> MealyT m a c
compose c p q = MealyT $ \a -> do
  (fb, q') <- runMealyT q a
  (c', p') <- run p c fb
  pure (c', compose c' p' q')

feedback :: (Monad m, Foldable f, Monoid (f a), Monoid (f b)) => MealyT m a (f b) -> MealyT m b (f a) -> MealyT m a (f b)
feedback m1 m2 = MealyT $ \a -> do
  -- run first machine
  (bs, m1') <- runMealyT m1 a

  -- run second machine
  (as, m2') <- run m2 mempty bs

  -- recursively run the whole machine with the states updated
  (bs', m12) <- run (feedback m1' m2') mempty as

  pure (bs <> bs', m12)
