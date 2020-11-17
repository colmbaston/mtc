module SrcPos (SrcPos(..), annotate) where

data SrcPos = SrcPos { line :: Int, column :: Int } deriving (Eq, Ord)

instance Show SrcPos where
  showsPrec _ (SrcPos l c) = showString "line " . shows l . showString ", column " . shows c

annotate :: String -> [(SrcPos, Char)]
annotate = go (SrcPos 1 1)
  where
    go :: SrcPos -> String -> [(SrcPos, Char)]
    go sp  []       = [(sp, '\ETX')]
    go sp ('\n':xs) = (sp, '\n') : go (SrcPos (line sp + 1)          1 ) xs
    go sp (   x:xs) = (sp,    x) : go (SrcPos (line sp) (column sp + 1)) xs
