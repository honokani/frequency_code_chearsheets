module PathControl where

import           Data.Char                    (toLower)
import qualified System.Directory      as SD
import qualified System.FilePath.Posix as SFP


data Exts = PNG
          | JPG
          | JPEG
          | GIF
          | WEBP
          | MD

getExtSpellings :: Exts -> [String]
getExtSpellings es = case es of
    JPG  -> [".jpg",".jpeg"]
    PNG  -> [".png"]
    GIF  -> [".gif"]
    WEBP -> [".webp"]
    MD   -> [".md"]


getCurrDirPath :: IO FilePath
getCurrDirPath = SD.getCurrentDirectory

join2paths :: FilePath -> String -> FilePath
join2paths path tgt = SFP.joinPath [path,tgt]

getContentNames :: FilePath -> IO ([FilePath],[FilePath])
getContentNames path = do
    cAll <- SD.getDirectoryContents path
    let c = filter (".." /=).filter ("." /=) $ cAll
    cBool <- mapM (SD.doesFileExist.join2paths path) c
    return $ distribute c cBool
    where
        distribute [] _ = ([],[])
        distribute (c:cs) (b:bs) = (ds,fs)
            where
                (xs,ys) = distribute cs bs
                (ds,fs) = if b then (xs,c:ys) else (c:xs,ys)

findFileByExt :: Exts -> ([FilePath],[FilePath]) -> [FilePath]
findFileByExt ex (_,fs) = found
    where
        found = filter (searchExt ex) fs
        searchExt ex f = elem (map toLower.snd.SFP.splitExtension $ f) $ getExtSpellings ex

