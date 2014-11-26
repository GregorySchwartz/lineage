lineage
=======

**Gregory W. Schwartz**

To install:
```
cabal update
cabal install
```

```
Usage: lineage (-i|--input FILE) [-c|--copyNumber] [-C|--inputCopyField INT]
               (-o|--output FILE)
  Create the lineage tree from a fasta file using shared mutations

Available options:
  -h,--help                Show this help text
  -i,--input FILE          The input fasta file, where the first entry is the
                           root
  -c,--copyNumber          Whether to take copy number into account for the
                           mutations
  -C,--inputCopyField INT  The field (1 indexed) in the header which contains
                           the copy number
  -o,--output FILE         The output file containing the json tree
```
