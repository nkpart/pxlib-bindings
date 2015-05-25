
import Foreign.Marshal.Array
import Foreign.Storable
import Paradox

example = do
  doc <- pXNew
  pXOpenFile doc "cube1_1data1.DB"
  numFields <- pXGetNumFields doc
  print =<< peekArray numFields =<< pXGetFields doc
  numRecords <- pXGetNumRecords doc
  record <- pXRetrieveRecord doc 3
  fields <- peekArray numFields record
  print =<< mapM peek fields
  putStrLn "Done"
  return ()
