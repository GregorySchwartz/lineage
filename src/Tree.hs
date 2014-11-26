-- Tree module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to the creation of the shared mutation
-- tree

module Tree where

-- Built-in
import qualified Data.Map as M
import Data.Tree

-- Local
import Types
import Utility

-- | Creat the subforest from the most common mutations
getSubForest :: [SuperFasta] -> [Tree TreeInfo]
getSubForest [] = []
getSubForest fastaList = createTree mutFreq (fst lineage)
                       : getSubForest (snd lineage)
  where
    lineage = iterateLineage (fst mutFreq) fastaList
    mutFreq = mostCommonMutation fastaList

-- | Create the lineage tree by finding the most common mutations
createTree :: (Mutation, Int) -> [SuperFasta] -> Tree TreeInfo
createTree mutFreq fastaList =
    Node { rootLabel = TreeInfo { sequences = map
                                              superFastaToPrintFasta
                                              fastaList
                                , mutation  = show . fst $ mutFreq
                                , number    = snd mutFreq }
         , subForest = getSubForest
                     . filter (not . M.null . mutations)
                     $ fastaList }
