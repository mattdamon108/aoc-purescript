module Parser where

import Prelude
import Data.Maybe (Maybe(..))

newtype Parser a
  = Parser (String -> Maybe a)

runParser :: forall a. Parser a -> String -> Maybe a
runParser (Parser g) s = g s

instance functorParser :: Functor Parser where
  map g f =
    Parser
      ( \s -> case runParser f s of
          Just v -> Just (g v)
          Nothing -> Nothing
      )

instance applyParser :: (Functor Parser) => Apply Parser where
  apply fg f =
    Parser
      ( \s -> case runParser fg s of
          Just g -> case runParser f s of
            Just v -> Just (g v)
            Nothing -> Nothing
          Nothing -> Nothing
      )

instance applicativeParser :: (Apply Parser) => Applicative Parser where
  pure x = Parser (\_ -> Just x)

instance bindParser :: (Apply Parser) => Bind Parser where
  bind m g =
    Parser
      ( \s -> case runParser m s of
          Just v -> runParser (g v) s
          Nothing -> Nothing
      )

instance monadParser :: (Bind Parser) => Monad Parser
