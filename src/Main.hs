-- lineage
-- By Gregory W. Schwartz

-- | Built-in
import qualified Data.ByteString.Lazy as B

-- | Cabal
import Options.Applicative
import Data.Fasta.String
import Data.Aeson

-- | Local
import Utility
import Tree

-- Command line arguments
data Options = Options { input          :: String
                       , copyFlag       :: Bool
                       , haskellFlag    :: Bool
                       , inputCopyField :: Int
                       , output         :: String
                       }

-- Command line options
options :: Parser Options
options = Options
      <$> strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> help "The input fasta file, where the first entry is the root" )
      <*> switch
          ( long "copyNumber"
         <> short 'c'
         <> help "Whether to take copy number into account for the mutations" )
      <*> switch
          ( long "haskell"
         <> short 'H'
         <> help "Whether to print the output as a haskell type" )
      <*> option auto
          ( long "inputCopyField"
         <> short 'C'
         <> value 1
         <> metavar "INT"
         <> help "The field (1 indexed) in the header\
                 \ which contains the copy number" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "The output file containing the json tree" )

sharedTree :: Options -> IO ()
sharedTree opts = do
    contents <- readFile . input $ opts

    let copyBool          = copyFlag opts
        copyIdx           = inputCopyField opts
        completeFastaList = parseFasta contents
        root              = toEmptySuperFasta . head $ completeFastaList
        fastaList         = map ( assignMutations root
                                . fastaToSuperFasta copyBool copyIdx)
                          . tail
                          $ completeFastaList
        tree              = createTree ((0, ('-', '-')), 0) fastaList

    if haskellFlag opts
        then writeFile (output opts) . show $ tree
        else B.writeFile (output opts) . encode $ tree

main :: IO ()
main = execParser opts >>= sharedTree
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Create the lineage tree from a fasta file using shared\
                 \ mutations"
     <> header "lineage, Gregory W. Schwartz" )
