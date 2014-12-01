-- Types module
-- By Gregory W. Schwartz

-- | Collects all types used in the program

{-# LANGUAGE DeriveGeneric #-}

module Types where

-- Built-in
import qualified Data.Map as M
import GHC.Generics
import qualified Data.Sequence as Seq

-- Cabal
import Data.Aeson

-- Algebraic
data TreeInfo = TreeInfo { sequences    :: [PrintFasta]
                         , nodeSequence :: String
                         , mutation     :: String
                         , number       :: Int }
                         deriving (Show, Generic)

data SuperFasta = SuperFasta { superFastaSeq    :: String
                             , superFastaHeader :: String
                             , copyNumber  :: Int
                             , mutations   :: M.Map Mutation Int }
                             deriving (Show, Generic)

data PrintFasta = PrintFasta { printFastaSeq    :: String
                             , printFastaHeader :: String }
                             deriving (Show, Generic)

instance FromJSON TreeInfo
instance ToJSON TreeInfo
instance FromJSON PrintFasta
instance ToJSON PrintFasta

-- Basic
type Position = Int

-- Advanced
type ParentSeq = Seq.Seq Char
type Mutation  = (Position, (Char, Char))
