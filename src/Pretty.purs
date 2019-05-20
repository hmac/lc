module Pretty where

-- Like show, but for pretty printing
class Pretty a where
  pretty :: a -> String
