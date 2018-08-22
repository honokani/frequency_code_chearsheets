module CommonUtil where


-----------------------
-- filter by multi conditions
-- e.g.
--   filter (((<)10)&&((>)3)||(==0)) xs
--   -> multiFilter cond xs
--          where
--              cond = O [A [C (<10),C (>3)], C (==0)]

data Condition a = C a
                 | A [Condition a]
                 | O [Condition a]

multiFilter :: Condition (a -> Bool) -> [a] -> [a]
multiFilter = filter.multiFilterCore
    where
        struct :: (Bool -> Bool -> Bool) -> (a -> Bool) -> [a -> Bool] -> (a -> Bool)
        struct sym = foldl (\acc x -> \t -> sym (x t) (acc t))
        multiFilterCore :: Condition (a -> Bool) -> (a -> Bool)
        multiFilterCore c = case c of
            (C f)  -> f
            (A fs) -> struct (&&) (\_ -> True)  $ map multiFilterCore fs
            (O fs) -> struct (||) (\_ -> False) $ map multiFilterCore fs

-----------------------

-- ternary operation
ternaryOp :: Bool -> (a,a) -> a
ternaryOp b (f,s) = if b then f else s

(<?) :: Bool -> (a,a) -> a
(<?) = ternaryOp





-----------------------
-- memo
--
--mFilter :: (Bool -> Bool -> Bool) -> [a -> Bool] -> [a] -> [a]
--mFilter sym conds = filter (struct sym (\_ -> True) conds)
--    where
--        struct sym = foldl (\acc x -> \t -> sym (x t) (acc t))
--
--mFilterA :: [a -> Bool] -> [a] -> [a]
--mFilterA c = mFilter (&&) c

