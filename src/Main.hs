-- lineage
-- By Gregory W. Schwartz

-- | Built-in
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Sequence as Seq

-- | Cabal
import Options.Applicative
import Data.Fasta.String
import Data.Aeson

-- | Local
import Types
import Utility
import Tree

-- Command line arguments
data Options = Options { input          :: String
                       , expandFlag     :: Bool
                       , copyFlag       :: Bool
                       , aaFlag         :: Bool
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
          ( long "expand-tree"
         <> short 'e'
         <> help "Whether to output the expanded tree with no collapsing of\
                 \ each mutation node" )
      <*> switch
          ( long "copy-number"
         <> short 'c'
         <> help "Whether to take copy number into account for the mutations" )
      <*> switch
          ( long "amino-acids"
         <> short 'a'
         <> help "Whether the sequences are DNA or proteins" )
      <*> switch
          ( long "haskell"
         <> short 'H'
         <> help "Whether to print the output as a haskell type" )
      <*> option auto
          ( long "input-copy-field"
         <> short 'C'
         <> value 1
         <> metavar "INT"
         <> help "The field (1 indexed) in the header\
                 \ which contains the copy number" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value ""
         <> help "The output file containing the json tree" )

sharedTree :: Options -> IO ()
sharedTree opts = do
    contents <- if (null . input $ opts)
                    then getContents
                    else readFile . input $ opts

    let copyBool          = copyFlag opts
        copyIdx           = inputCopyField opts
        completeFastaList = parseFasta contents
        root              = toEmptySuperFasta . head $ completeFastaList
        fastaList         = map ( assignMutations (aaFlag opts) root
                                . fastaToSuperFasta copyBool copyIdx)
                          . tail
                          $ completeFastaList
        tree              = createTree Nothing
                            (Seq.fromList . superFastaSeq $ root)
                            fastaList
        finalTree         = if expandFlag opts
                                then tree
                                else collapseTree [] tree

    -- Output results to stdin or file
    if (null . output $ opts)
        then
            if haskellFlag opts
                then putStrLn . show $ finalTree
                else B.putStrLn . encode $ finalTree
        else
            if haskellFlag opts
                then writeFile (output opts) . show $ finalTree
                else B.writeFile (output opts) . encode $ finalTree

main :: IO ()
main = execParser opts >>= sharedTree
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Create the lineage tree from a fasta file using shared\
                 \ mutations"
     <> header "lineage, Gregory W. Schwartz" )
