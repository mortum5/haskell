module Monoids where

import           Data.Semigroup

data NonEmpry a = a :| [a]
