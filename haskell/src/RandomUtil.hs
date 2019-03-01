import System.Random

getRndList :: (Random a) => (a, a) -> Int -> IO [a]
getRndList range n = sequence.replicate n $ randomRIO range

