-- Tree module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to the creation of the shared mutation
-- tree

module Tree where

-- Built-in
import qualified Data.Map as M
import Data.Tree
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F

-- Local
import Types
import Utility

-- | Creat the subforest from the most common mutations
getSubForest :: ParentSeq -> [SuperFasta] -> [Tree TreeInfo]
getSubForest _ [] = []
getSubForest parentSeq fastaList = createTree mutFreq parentSeq (fst lineage)
                                 : getSubForest parentSeq (snd lineage)
  where
    lineage = iterateLineage (fst mutFreq) fastaList
    mutFreq = mostCommonMutation fastaList

-- | Create the lineage tree by finding the most common mutations
createTree :: (Mutation, Int) -> ParentSeq -> [SuperFasta] -> Tree TreeInfo
createTree mutFreq parentSeq fastaList =
    Node { rootLabel = TreeInfo { sequences = map
                                              superFastaToPrintFasta
                                              fastaList
                                , nodeSequence = F.toList newSeq
                                , mutation  = show . fst $ mutFreq
                                , number    = snd mutFreq }
         , subForest = getSubForest newSeq
                     . filter (not . M.null . mutations)
                     $ fastaList }
  where
    newSeq = mutate mutFreq parentSeq
    mutate ((0, ('-', '-')), 0) = id
    mutate ((p, (_, x)), _)     = Seq.update p x
