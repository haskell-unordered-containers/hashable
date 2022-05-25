{-# LANGUAGE DeriveGeneric #-}
import Data.Hashable
import Data.Hashable.Lifted
import GHC.Generics (Generic)
import Data.Hashable (fnvHash)

data Foo
  = Foo1 Int Char Bool
  | Foo2 String ()
  deriving (Eq, Generic)

instance Hashable Foo

data Bar = Bar Double Float
  deriving (Eq, Generic)

instance Hashable Bar

-- printHash :: (Hashable a, Show a) => a -> IO ()
-- printHash = print . hash

main :: IO ()
main = do
  putStrLn "Hashing Foo1"
  print . fnvHash $ Foo1 22 'y' True
  putStrLn "Hashing Foo2"
  print . fnvHash $ Foo2 "hello" ()
  putStrLn "Hashing Bar"
  print . fnvHash $ Bar 55.50 9.125
  pure ()

-----------------------------------
-- Higher Rank Hashable Examples --
-----------------------------------

{- TODO:

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
data Free f a = Pure a | Free (f (Free f a))

instance (Hashable w, Hashable1 m) => Hashable1 (WriterT w m) where
    liftHashWithSalt h s (WriterT m) =
        liftHashWithSalt (liftHashWithSalt2 h hashWithSalt) s m
instance Hashable1 f => Hashable1 (Free f) where
    liftHashWithSalt h = go where
        go s x = case x of
            Pure a -> h s a
            Free p -> liftHashWithSalt go s p

instance (Hashable w, Hashable1 m, Hashable a) => Hashable (WriterT w m a) where
    hashWithSalt = hashWithSalt1
instance (Hashable1 f, Hashable a) => Hashable (Free f a) where
    hashWithSalt = hashWithSalt1
-}
