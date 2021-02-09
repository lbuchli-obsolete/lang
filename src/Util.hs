{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Util where

import           Control.Applicative
import           Control.Monad
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromMaybe )
import           Data.Bifunctor

----------------------------------------------------------------------
--                              Result                              --
----------------------------------------------------------------------

data Result e a = Success a | Trace String (Result e a) | Error e

instance Functor (Result e) where
  fmap f (Success a  ) = Success (f a)
  fmap f (Trace msg r) = Trace msg $ fmap f r
  fmap _ (Error msg  ) = Error msg

instance Bifunctor Result where
  bimap _  fa (Success a  ) = Success (fa a)
  bimap fe fa (Trace msg r) = Trace msg $ bimap fe fa r
  bimap fe _  (Error msg  ) = Error (fe msg)

instance Applicative (Result e) where
  pure x = Success x
  (<*>) (Success f ) (Success a)  = Success (f a)
  (<*>) (Trace _ rf) ra           = rf <*> ra
  (<*>) (Error msg ) _            = Error msg
  (<*>) rf           (Trace _ ra) = rf <*> ra
  (<*>) _            (Error msg ) = Error msg

instance Monad (Result e) where
  (>>=) (Success a  ) f = f a
  (>>=) (Trace msg a) f = Trace msg (a >>= f)
  (>>=) (Error msg  ) _ = Error msg

instance Alternative (Result e) where
  empty = Error (error "Error")
  (<|>) (Trace m a) b           = Trace m $ a <|> b
  (<|>) (Success a) _           = Success a
  (<|>) (Error   a) (Trace m b) = Trace m $ Error a <|> b
  (<|>) (Error   _) (Success b) = Success b
  (<|>) (Error   a) (Error   _) = Error a

instance Eq a => Eq (Result e a) where
  (==) (Success a    ) (Success b)     = a == b
  (==) (Success _    ) _               = False
  (==) (Trace msg_a a) (Trace msg_b b) = msg_a == msg_b && a == b
  (==) (Trace _     _) _               = False
  (==) (Error _      ) (Error _)       = True
  (==) (Error _      ) _               = False

instance (Show e, Show a) => Show (Result e a) where
  show (Success a  ) = "Success " ++ show a
  show (Trace msg a) = "Trace '" ++ msg ++ "'\n" ++ show a
  show (Error e    ) = "Error: " ++ show e

type ShowTrace = Bool
printResult :: (Show e, Show a) => ShowTrace -> Result e a -> IO ()
printResult _    (Error   e  ) = print e
printResult _    (Success s  ) = print s
printResult True (Trace msg r) = do
  putStr "TRACE: "
  putStrLn msg
  printResult True r
printResult False (Trace _ r) = printResult False r

noTrace :: Result e a -> Result e a
noTrace (Success a) = Success a
noTrace (Trace _ a) = noTrace a
noTrace (Error e  ) = Error e

successOrDefault :: Result e a -> a -> a
successOrDefault (Success a) _ = a
successOrDefault (Trace _ x) a = successOrDefault x a
successOrDefault (Error _  ) a = a

toEither :: Result e a -> Either e a
toEither (Success a) = Right a
toEither (Trace _ a) = toEither a
toEither (Error e  ) = Left e

fromMaybe :: Maybe a -> Result String a
fromMaybe (Just a) = Success a
fromMaybe Nothing  = Error "No value"

----------------------------------------------------------------------
--                              Parser                              --
----------------------------------------------------------------------

{-|
A position in the source code.
-}
data Pos = Pos {
  row :: Int,
  col :: Int
} deriving (Eq, Show)

addPos :: Pos -> Pos -> Pos
addPos (Pos x y) (Pos x' y') = Pos (x + x') (y + y')

posOffsetOf :: String -> Pos
posOffsetOf s = Pos (length s - Data.Maybe.fromMaybe 0 (elemIndex '\n' s))
                    (length $ filter (== '\n') s)

type State i e a = Result (Pos, e) (i, Pos, a)

{-|
Parser; Parses source code and spits out some structured representation of it
or an error if parsing the source wasn't possible
-}
newtype Parser i e a = Parser {
  parseSrc :: i -> State i e a
}

instance Functor (Parser i e) where
  fmap f b = Parser $ \i -> fmap (\(i', p, a) -> (i', p, f a)) (parseSrc b i)

instance Applicative (Parser i e) where
  pure x = Parser $ \i -> pure (i, Pos 0 0, x)
  (<*>) f g = Parser $ (>>= parse_g) . parseSrc f
   where
    parse_g (i', pf, func) = bimap
      (\(pg, msg) -> (addPos pf pg, msg))
      (\(i'', pg, a) -> (i'', addPos pf pg, func a))
      (parseSrc g i')

instance Alternative (Parser i e) where
  empty = Parser $ const empty
  (<|>) a b = Parser $ \i -> parseSrc a i <|> parseSrc b i

instance Monad (Parser i e) where
  (>>=) pa pb = Parser $ parseSrc pa >=> \(i', p, a) ->
    fmap (\(i'', p', b) -> (i'', addPos p p', b)) (parseSrc (pb a) i')

---------------------------------------------------------------------------
--                                 Other                                 --
---------------------------------------------------------------------------

apply :: [a -> a] -> a -> a
apply (f : fs) a = apply fs (f a)
apply []       a = a
