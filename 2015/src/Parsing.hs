module Parsing where

import Data.Char
import Control.Monad

newtype Parser a        = P (String -> [(a, String)])