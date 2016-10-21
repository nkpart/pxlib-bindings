
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Foreign.C
import Paradox
import Debug.Trace

data Field = Field String CChar CInt CInt deriving (Eq, Show)

z :: IO a -> (a -> b) -> IO b
z = flip fmap

getFields :: PXDocPtr -> IO ()
getFields doc = do
  numFields <- pXGetNumFields doc
  pxfs <- peekArray numFields =<< pXGetFields doc
  print pxfs
  traverse (\(PXField cs _ _ _) -> (=<<) print $ trace "cs" (return cs) `z` (\s -> trace s s)) pxfs
  return ()

main :: IO ()
main = do
  doc <- pXNew
  pXOpenFile doc "example/cube1_1data1.DB"
  numFields <- pXGetNumFields doc
  getFields doc
  print =<< peekArray numFields =<< pXGetFields doc
  numRecords <- pXGetNumRecords doc
  record <- pXRetrieveRecord doc 3
  fields <- peekArray numFields record
  print =<< mapM peek fields
  putStrLn "Done"
  return ()
