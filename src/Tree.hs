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
getSubForest parentSeq fastaList =
        createTree (Just mutFreq) parentSeq (fst lineage)
      : getSubForest parentSeq (snd lineage)
  where
    lineage = iterateLineage (fst mutFreq) fastaList
    mutFreq = mostCommonMutation fastaList

-- | Create the lineage tree by finding the most common mutations
createTree :: Maybe (Mutation, Int)
           -> ParentSeq
           -> [SuperFasta]
           -> Tree TreeInfo
createTree mutFreq parentSeq fastaList =
    Node { rootLabel = TreeInfo { sequences = map
                                              superFastaToPrintFasta
                                              fastaList
                                , nodeSequence  = F.toList newSeq
                                , nodeMutations = (: [])
                                . printMutation
                                $ mutFreq
                                , number    = printNumber mutFreq }
         , subForest = getSubForest newSeq
                     . filter (not . M.null . mutations)
                     $ fastaList }
  where
    printNumber Nothing = 0
    printNumber (Just (_, x)) = x
    printMutation Nothing = ""
    printMutation (Just (x, _)) = show x
    newSeq = mutate mutFreq parentSeq
    mutate Nothing = id
    mutate (Just ((p, (_, x)), _))     = Seq.update (p - 1) x

-- | Collapse nodes where there are no observed sequences, as we don't know
-- what order the mutations happened in
collapseTree :: [String] -> Tree TreeInfo -> Tree TreeInfo
collapseTree _ tree@(Node { rootLabel = TreeInfo { nodeMutations = [""] }
                          , subForest = ts }) =
    tree { subForest = map (collapseTree []) ts }
collapseTree _ tree@(Node { subForest = [] }) = tree
collapseTree muts tree@(Node { rootLabel = rl, subForest = ts })
    | any (== 0) . map remainingMutations . sequences $ rl =
        tree { rootLabel = rl { nodeMutations = nodeMutations rl ++ muts }
             , subForest = map (collapseTree []) ts }
    | (all (/= 0) . map remainingMutations . sequences $ rl)
   && (null . tail $ ts) = collapseTree (muts ++ nodeMutations rl) . head $ ts
    | otherwise = tree { subForest = map (collapseTree []) ts }
