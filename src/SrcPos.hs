module SrcPos (SrcPos(..), nextColumn, nextLine, annotate) where

data SrcPos = SrcPos Int Int deriving (Eq, Ord)

instance Show SrcPos where
  showsPrec _ (SrcPos l c) = showString "line " . shows l . showString ", column " . shows c

nextColumn :: SrcPos -> SrcPos
nextColumn (SrcPos l c) = SrcPos l (c+1)

nextLine :: SrcPos -> SrcPos
nextLine (SrcPos l _) = SrcPos (l+1) 1

annotate :: String -> [(SrcPos, Char)]
annotate = go (SrcPos 1 1)
  where
    go :: SrcPos -> String -> [(SrcPos, Char)]
    go sp  []       = [(sp, '\ETX')]
    go sp ('\n':xs) =  (sp, '\n') : go (nextLine   sp) xs
    go sp (   x:xs) =  (sp,    x) : go (nextColumn sp) xs
