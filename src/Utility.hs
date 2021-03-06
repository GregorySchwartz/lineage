-- Utility module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to general purpose in the program

{-# LANGUAGE BangPatterns #-}

module Utility where

-- Built-in
import Data.List
import qualified Data.Map as M
import Data.Function (on)

-- Cabal
import Data.Fasta.String
import qualified Data.List.Split as Split

-- Local
import Types

-- | Convert a SuperFasta to a PrintFasta, for easy saving to JSON
superFastaToPrintFasta :: SuperFasta -> PrintFasta
superFastaToPrintFasta ( SuperFasta { superFastaSeq    = x
                                    , superFastaHeader = y
                                    , mutations        = z } ) =
    PrintFasta { printFastaSeq      = x
               , printFastaHeader   = y
               , remainingMutations = M.size z}

-- | Convert a FastaSequence to a SuperFasta, with mutations and copy number.
-- IMPORTANT: I get the mutations later as I need the copy number first
fastaToSuperFasta :: Bool -> Int -> FastaSequence -> SuperFasta
fastaToSuperFasta copyBool
                  copyIdx
                  f@(FastaSequence { fastaSeq = x, fastaHeader = y }) =
    SuperFasta { superFastaSeq    = x
               , superFastaHeader = y
               , copyNumber       = getCopyNumber copyBool copyIdx f
               , mutations        = M.empty }

-- | Needed to compare germline to other super fastas
toEmptySuperFasta :: FastaSequence -> SuperFasta
toEmptySuperFasta (FastaSequence { fastaSeq = x, fastaHeader = y }) =
    SuperFasta { superFastaSeq    = x
               , superFastaHeader = y
               , copyNumber       = 1
               , mutations        = M.empty }

-- | Assign mutations to a SuperFasta
assignMutations :: Bool -> SuperFasta -> SuperFasta -> SuperFasta
assignMutations aaBool germline f = f { mutations = M.fromListWith (+)
                                                 . getMutations aaBool germline
                                                 $ f }

-- | Get the copy number of a FastaSequence
getCopyNumber :: Bool -> Int -> FastaSequence -> Int
getCopyNumber False _ _ = 1
getCopyNumber True copyIdx f = read
                             . (!! (copyIdx - 1))
                             . Split.splitOn "|"
                             . fastaHeader
                             $ f

-- | Get the most common mutation from a list of SuperFasta
mostCommonMutation :: [SuperFasta] -> (Mutation, Int)
mostCommonMutation = maximumBy (compare `on` snd)
                   . M.toAscList
                   . M.unionsWith (+)
                   . map mutations

-- | Get the list of comparisons from two fasta sequences with the copy number
-- of the second sequence
getComparisons :: SuperFasta
               -> SuperFasta
               -> [(Mutation, Int)]
getComparisons x y = zip (zip [1..] . zip (superFastaSeq x) $ superFastaSeq y)
                   . repeat
                   . copyNumber
                   $ y

-- | Get the list of mutations from two fasta sequences with the copy number
-- of the second sequence, ignoring "fake" mutations
getMutations :: Bool
             -> SuperFasta
             -> SuperFasta
             -> [(Mutation, Int)]
getMutations aaBool x y = filter (pureMutation aaBool . fst)
                        $ getComparisons x y

-- | Get real mutations from a list of mutations
pureMutation :: Bool -> Mutation -> Bool
pureMutation aaBool (_, (m1, m2)) = (not . isBad aaBool $ m1)
                                 && (not . isBad aaBool $ m2)
                                 && (m1 /= m2)
  where
    isBad True x  = x `elem` "-."
    isBad False x = x `elem` "Nn-."

-- | Find the sequences with a mutation
hasMut :: Mutation -> SuperFasta -> Bool
hasMut mut = M.member mut . mutations

-- | Remove mutation from a sequence
removeMutFromFasta :: Mutation -> SuperFasta -> SuperFasta
removeMutFromFasta mut f = f { mutations = M.delete mut . mutations $ f }

-- | Get the fasta sequences with a mutation in them, removing that mutation, as
-- well as the rest of the sequences
iterateLineage :: Mutation -> [SuperFasta] -> ([SuperFasta], [SuperFasta])
iterateLineage !mut !xs = (withMut, rest)
  where
    withMut = map (removeMutFromFasta mut) . filter (hasMut mut) $ xs
    rest    = filter (not . hasMut mut) xs
