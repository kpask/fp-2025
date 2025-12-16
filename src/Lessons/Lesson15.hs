{-# LANGUAGE InstanceSigs #-}
module Lessons.Lesson15 where
import qualified Data.List as L

import Data.Char (isAlpha)

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Strict (State, get, put, runState)


newtype EitherT e m a = EitherT {
  runEitherT :: m (Either e a)
}

instance Monad m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f ta = EitherT $ do
    a <- runEitherT ta
    case a of
      Left e -> return $ Left e
      Right r -> return $ Right $ f r

instance Monad m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ return $ Right a
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  tf <*> ta = EitherT $ do
    f <- runEitherT tf
    case f of
      Left e1 -> return $ Left e1
      Right r1 -> do
        a <- runEitherT ta
        case a of
          Left e2 -> return $ Left e2
          Right r2 -> return $ Right (r1 r2)

instance Monad m => Monad (EitherT e m) where
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  m >>= k = EitherT $ do
    a <- runEitherT m
    case a of
      Left e -> return $ Left e
      Right r -> runEitherT (k r)

type Parser a = EitherT String (State String) a

throwE :: String -> Parser a
throwE msg = EitherT $ return $ Left msg

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift ma = EitherT $ fmap Right ma


-- >>> :t runEitherT parseLetter
-- runEitherT parseLetter :: State String (Either String Char)
-- >>> :t runState (runEitherT parseLetter)
-- runState (runEitherT parseLetter) :: String -> (Either String Char, String)
-- >>> runState (runEitherT parseLetter) ""
-- (Left "A letter is expected but got empty input","")
-- >>> runState (runEitherT parseLetter) "13123"
-- (Left "A letter is expected, but got 1","13123")
-- >>> runState (runEitherT parseLetter) "abba"
-- (Right 'a',"bba")
parseLetter :: Parser Char
parseLetter = do
  input <- lift get
  case input of
    [] -> throwE "A letter is expected but got empty input"
    (h:t) -> if isAlpha h
    then do
      lift (put t)
      return h
    else throwE $ "A letter is expected, but got " ++ [h]

-- >>> runState (runEitherT twoLettersM) "abba"
-- (Right ('a','b'),"ba")
twoLettersM :: Parser (Char, Char)
twoLettersM = do
  a <- parseLetter
  b <- parseLetter
  return (a, b)

-- >>> runState (runEitherT twoLettersA) "abba"
-- (Right ('a','b'),"ba")
twoLettersA :: Parser (Char, Char)
twoLettersA = (,) <$> parseLetter <*> parseLetter

