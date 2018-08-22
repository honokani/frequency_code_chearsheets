module Lib
    ( someFunc
    ) where

import System.Directory    as SD
-- my modules
import CommonUtil          as CU
import PathControl         as PC


someFunc :: IO ()
someFunc = do
    print =<< PC.findFileByExt PC.MD <$> (PC.getContentNames =<< PC.getCurrDirPath)
    print $ CU.multiFilter (A [C (==0), C (\x -> x+1==1)]) [0,1,2,0]
    print $ (\_ -> True) () <? (100,200)

