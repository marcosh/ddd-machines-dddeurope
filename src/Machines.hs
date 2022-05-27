{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Machines where

import qualified Control.Arrow as Arrow ((***))
import qualified Control.Category as Cat (Category (id, (.)))
import Data.Bifunctor (first)
import Data.Foldable (foldlM)

-- profunctors
import Data.Profunctor

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

instance Functor m => Profunctor (MealyT m) where
  lmap :: (a -> b) -> MealyT m b c -> MealyT m a c
  lmap f (MealyT m) = MealyT $ fmap (fmap $ lmap f) <$> m . f

  rmap :: (b -> c) -> MealyT m a b -> MealyT m a c
  rmap f (MealyT m) = MealyT $ fmap (f Arrow.*** rmap f) <$> m

instance Functor m => Strong (MealyT m) where
  first' :: MealyT m a b -> MealyT m (a, c) (b, c)
  first' (MealyT m) = MealyT $ \(a, c) -> ((, c) Arrow.*** first') <$> m a

(***) :: (Cat.Category p, Strong p) => p a b -> p c d -> p (a, c) (b, d)
(***) pab pcd = second' pcd Cat.. first' pab

(&&&) :: (Cat.Category p, Strong p) => p a b -> p a c -> p a (b, c)
(&&&) pab pac = (pab *** pac) Cat.. dimap id (\a -> (a, a)) Cat.id

mealyT :: Functor m => (s -> a -> m (b, s)) -> s -> MealyT m a b
mealyT f = MealyT . (fmap . fmap . fmap $ mealyT f) . f

type Mealy a b = forall m . Monad m => MealyT m a b

mealy :: (s -> a -> (b, s)) -> s -> Mealy a b
mealy f s = (mealyT . (fmap . fmap $ pure)) f s

statefulT :: Functor m => (s -> a -> m s) -> s -> MealyT m a s
statefulT = mealyT . ((fmap (\a -> (a, a)) .) .)

stateful :: (s -> a -> s) -> s -> Mealy a s
stateful f s = (statefulT . (fmap . fmap $ pure)) f s

mooreT :: Functor m => (s -> m (b, a -> s)) -> s -> MealyT m a b
mooreT f = mealyT (\s a -> fmap ($ a) <$> f s)

moore :: (s -> (b, a -> s)) -> s -> Mealy a b
moore f s = (mooreT . (\f' s' -> pure (($) <$> f' s'))) f s

statelessT :: Functor m => (a -> m b) -> MealyT m a b
statelessT f = mealyT (\() a -> (, ()) <$> f a) ()

stateless :: (a -> b) -> Mealy a b
stateless f = (statelessT . (pure .)) f

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
