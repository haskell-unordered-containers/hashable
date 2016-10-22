{-# LANGUAGE DeriveGeneric #-}
import Data.Hashable
import GHC.Generics (Generic)

data Foo
  = Foo1 Int Char Bool
  | Foo2 String ()
  deriving (Generic)

instance Hashable Foo

data Bar = Bar Double Float
  deriving (Generic)

instance Hashable Bar

-- printHash :: (Hashable a, Show a) => a -> IO ()
-- printHash = print . hash

main :: IO ()
main = do
  putStrLn "Hashing Foo1"
  print . hash $ Foo1 22 'y' True
  putStrLn "Hashing Foo2"
  print . hash $ Foo2 "hello" ()
  putStrLn "Hashing Bar"
  print . hash $ Bar 55.50 9.125
