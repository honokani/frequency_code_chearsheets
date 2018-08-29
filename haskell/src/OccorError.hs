{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module OccorError where

data DirSet a = Ds { app  :: a
                   , ress :: a
                   , pics :: a
                   } deriving (Show, Functor, Foldable, Traversable)
type DirName = String
data DirPos a = Root   DirName
              | Branch DirName a
dirSet :: DirSet (DirPos (DirSet a -> a))
dirSet = Ds { app  = Root   "app"
            , ress = Branch "ress" app
            , pics = Branch "pics" ress
            }

structPaths :: DirSet (DirPos a) -> DirSet FilePath
structPaths ds = fmap structPath ds
    where
        structPath d = case d of
            Root   n   -> n
            --Branch n f -> structPath (f ds) ++ "/" ++ n
            Branch n _ -> structPath (app ds) ++ "/" ++ n


-- window start.
main :: IO ()
main = do
    print $ structPaths dirSet

