
module Parsing where

import Data.Char
import Control.Monad

--infixr 5 +++
-- The monad of parsers

newtype Parser a 		=  P (String -> [(a,String)])

instance Monad Parser where
	return v			= P (\inp -> [(v,inp)])
	p >>= f				= case p of
							[] -> []
							[(v,inp)] -> f inp

instance MonadPlus Parser where
	mzero 				= P (\inp -> [])
	m `mplus` q			= P (\inp -> case parse p inp of
										[]			-> parse q inp
										[(v,out)])	-> [(v,out)]
-- Basic parsers
failure					:: Parser a	
failure					= P (\_ -> [])

return :: a -> Parser a
return v = P(\inp -> [(v, inp)])

item					:: Parser Char
item					= P (\inp -> case inp of
										[]		-> []
										(x:xs)	-> [(x,xs)])