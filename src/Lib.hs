module Lib
    ( Syntax, parse, display
    , (<$$>), (<**>), (<||>)
    , phantom, opt, many, many1
    , token, match, digit, integer
    ) where
import Data.Bifunctor (first)
import Data.Default (Default, def)
import Data.Char (isDigit)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))

data Prism s a = Prism { inj :: a -> s
                       , tryProj :: s -> Maybe a }

-- ---

class PrismFunctor f where
    pmap :: Prism s a -> f a -> f s

(<$$>) :: PrismFunctor f => Prism s a -> f a -> f s
infixl 4 <$$>
(<$$>) = pmap

class Sequential f where
    with :: f a -> f b -> f (a, b)

(<**>) :: Sequential f => f a -> f b -> f (a, b)
infixl 5 <**>
(<**>) = with

(**>) :: (PrismFunctor f, Sequential f) => f () -> f a -> f a
infixl 5 **>
s **> s' = Prism {inj = \(_, x) -> x, tryProj = Just . ((),)} <$$> s <**> s'

class Alternative f where
    orElse :: f a -> f a -> f a
    empty :: f a

(<||>) :: Alternative f => f a -> f a -> f a
infixl 3 <||>
(<||>) = orElse

-- ---

type Parser e a = String -> Either e (a, String)

type Printer a = a -> Maybe String

data Syntax e a = Syntax { parse :: Parser e a
                         , display :: Printer a }

instance PrismFunctor (Syntax e) where
    pmap (Prism {inj, tryProj}) (Syntax {parse, display}) =
        Syntax { parse = fmap (first inj) . parse
               , display = tryProj >=> display }

instance Sequential (Syntax e) where
    with s s' = Syntax {parse = parse'', display = display''}
        where parse'' input = do (l, input) <- parse s input
                                 (r, input) <- parse s' input
                                 return ((l, r), input)
              display'' (l, r) = (<>) <$> (display s l) <*> (display s' r)

instance Default e => Alternative (Syntax e) where
    orElse (Syntax {parse, display}) (Syntax {parse = parse', display = display'}) =
        Syntax {parse = parse'', display = display''}
        where parse'' input = case parse input of
                                  Left _ -> parse' input
                                  res -> res
              display'' v = display v <|> display' v -- laziness FTW

    empty = Syntax { parse = \_ -> Left def
                   , display = const Nothing }

phantom :: Eq a => a -> Syntax e a
phantom v = Syntax { parse = Right . (v,)
                   , display = \v' -> if v' == v then Just mempty else Nothing }

token :: Default e => Syntax e Char
token = Syntax { parse = \case
                              tok : input -> Right (tok, input)
                              [] -> Left def
               , display = Just . pure }

-- ---

just :: Prism (Maybe a) a
just = Prism {inj = Just, tryProj = id}

nil :: Prism [a] ()
nil = Prism { inj = const []
            , tryProj = \case
                             _ : _ -> Nothing
                             [] -> Just () }

cons :: Prism [a] (a, [a])
cons = Prism { inj = \(x, xs) -> x : xs
             , tryProj = \case
                              x : xs -> Just (x, xs)
                              [] -> Nothing }

-- Uses `read`, so only use on strings that have been seen to be safe by the parser.
readShow :: (Read a, Show a) => Prism a String
readShow = Prism {inj = read, tryProj = Just . show}

-- ---

match :: Default e => (Char -> Bool) -> Syntax e Char
match p = Syntax { parse = \case
                                t : input | p t -> Right (t, input)
                                _ : _ -> Left def
                                [] -> Left def
                 , display = \v -> if p v then Just [v] else Nothing }

opt :: (Eq a, Default e) => Syntax e a -> Syntax e (Maybe a)
opt s = just <$$> s
      <||> phantom Nothing

many, many1 :: (Default e, Eq a) => Syntax e a -> Syntax e [a]
many s = many1 s <||> phantom []
many1 s = cons <$$> s <**> many s

-- ---

digit :: Default e => Syntax e Char
digit = match isDigit

integer :: Default e => Syntax e Integer
integer = readShow <$$> many1 digit -- an example of sane `readShow` usage

