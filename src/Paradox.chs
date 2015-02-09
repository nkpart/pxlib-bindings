module Paradox where

#include "paradox.h"

import Foreign.C
import Foreign.C.String
import Control.Applicative
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

{#fun pure PX_get_majorversion as ^ {  } -> `Int' #}
{#fun pure PX_get_minorversion as ^ {  } -> `Int' #}
{#fun pure PX_get_subminorversion as ^ {  } -> `Int' #}
{#fun PX_boot as ^ { } -> `()' #}
{#fun PX_shutdown as ^ { } -> `()' #}

data PXDoc
{#pointer *pxdoc_t as PXDocPtr -> PXDoc #}

{#fun PX_new as ^ { } -> `PXDocPtr' #}
{#fun PX_open_file as ^ { `PXDocPtr', `String' } -> `Int' #}

data PXField = PXField CString CChar CInt CInt deriving (Eq, Show)
{#pointer *pxfield_t as PXFieldPtr -> PXField#}

instance Storable PXField where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = {#sizeof pxfield_t#}
  peek p =
    PXField <$> ({#get pxfield_t.px_fname #} p)
            <*> ({#get pxfield_t.px_ftype #} p)
            <*> ({#get pxfield_t.px_flen #} p)
            <*> ({#get pxfield_t.px_fdc #} p)
  poke p (PXField a b c d) = do
    {#set pxfield_t.px_fname #} p a
    {#set pxfield_t.px_ftype #} p (b)
    {#set pxfield_t.px_flen #}  p (c)
    {#set pxfield_t.px_fdc #}   p (d)

-- PX_create_file(pxdoc_t *pxdoc, pxfield_t *pxf, int numfields, const char *filename, int type);
{#fun PX_create_file as ^ { `PXDocPtr', withArray* `[PXField]', `Int', `String', `Int' } -> `Int' #}

{-
int PX_put_recordn(pxdoc_t *pxdoc, char *data, int recpos);
int PX_put_record(pxdoc_t *pxdoc, char *data);
int PX_insert_record(pxdoc_t *pxdoc, pxval_t **dataptr);
int PX_update_record(pxdoc_t *pxdoc, pxval_t **dataptr, int recno);
int PX_delete_record(pxdoc_t *pxdoc, int recno);
-}

data PXVal = PXVal (Maybe PXValValue) deriving (Eq, Show)
data PXValValue = LVal CLong | DVal CDouble | Str CString CInt deriving (Eq, Show)

{#pointer *pxval_t as PXValPtr -> PXVal#}

-- The destruction of PXVal is defined here: http://pxlib.sourceforge.net/documentation.php?manpage=PX_retrieve_record
-- "The paradox field types pxfShort, pxfLong, pxfDate, pxfTime, pxfLogical, and pxfAutoInc are returned as long int values. pxfTimestamp, pxfNumber, and pxfCurrency are returned as double values and all remaining paradox field types are stored as strings with the length in value.len"
instance Storable PXVal where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = {#sizeof pxval_t#}
  peek p = let longType = LVal <$> {#get pxval_t.value.lval #} p
               doubleType = DVal <$> {#get pxval_t.value.dval #} p
               stringType = Str <$> {#get pxval_t.value.str.val #} p <*> {#get pxval_t.value.str.len #} p
   in

    do
    v <- {#get pxval_t.isnull #} p
    case v of
      1 -> return . PXVal $ Nothing
      0 -> do tp <- {#get pxval_t.type #} p
              val <- case tp of
                {#const pxfAlpha        #} -> stringType
                {#const pxfDate         #} -> longType
                {#const pxfShort        #} -> longType
                {#const pxfLong         #} -> longType
                {#const pxfCurrency     #} -> doubleType
                {#const pxfNumber       #} -> doubleType
                {#const pxfLogical      #} -> longType
                {#const pxfMemoBLOb     #} -> stringType
                {#const pxfBLOb         #} -> stringType
                {#const pxfFmtMemoBLOb  #} -> stringType
                {#const pxfOLE          #} -> stringType
                {#const pxfGraphic      #} -> stringType
                {#const pxfTime         #} -> longType
                {#const pxfTimestamp    #} -> doubleType
                {#const pxfAutoInc      #} -> longType
                {#const pxfBCD          #} -> stringType
                {#const pxfBytes        #} -> stringType
                {#const pxfNumTypes     #} -> stringType
                _ -> error $ "Unknown paradox field type " ++ show tp
              return . PXVal . Just $ val

  -- poke p (PXVal Nothing) = do {#set pxval_t.isnull #} p 1
  --                             {#set pxval_t.type #} p 0
  --                             {#set pxval_t.value.lval #} p 0
  -- TODO: Poking is hard. What if the type has changed. We really need an ADT for each of the types
  -- poke p (PXVal (Just v)) = do {#set pxval_t.isnull #} p 0
  --                              case v of
  --                                LVal l -> {#set pxval_t.type #} p 0
  --                              {#set pxval_t.value.lval #} p 0

{#fun PX_retrieve_record as ^ { `PXDocPtr', `Int' } -> `Ptr PXValPtr' id #}
{-
pxval_t ** PX_retrieve_record(pxdoc_t *pxdoc, int recno);
-}

{#fun PX_close as ^ { `PXDocPtr' } -> `()' #}
{#fun PX_delete as ^ { `PXDocPtr' } -> `()' #}
{#fun PX_pack as ^ { `PXDocPtr' } -> `Int' #}
{#fun PX_get_fields as ^ { `PXDocPtr' } -> `PXFieldPtr' #}
{#fun PX_get_field as ^ { `PXDocPtr', `Int' } -> `PXFieldPtr' #}
{#fun PX_get_num_fields as ^ { `PXDocPtr' } -> `Int' #}
{#fun PX_get_num_records as ^ { `PXDocPtr' } -> `Int' #}
{#fun PX_get_recordsize as ^ { `PXDocPtr' } -> `Int' #}
{-
int PX_set_parameter(pxdoc_t *pxdoc, const char *name, const char *value);
int PX_get_parameter(pxdoc_t *pxdoc, const char *name, char **value);
int PX_set_value(pxdoc_t *pxdoc, const char *name, float value);
int PX_get_value(pxdoc_t *pxdoc, const char *name, float *value);
int PX_set_targetencoding(pxdoc_t *pxdoc, const char *encoding);
int PX_set_inputencoding(pxdoc_t *pxdoc, const char *encoding);
int PX_set_tablename(pxdoc_t *pxdoc, const char *tablename);
/* Data conversion functions */
/* Funktion to add data to a record */
void PX_put_data_alpha(pxdoc_t *pxdoc, char *data, int len, char *value);
void PX_put_data_bytes(pxdoc_t *pxdoc, char *data, int len, char *value);
void PX_put_data_double(pxdoc_t *pxdoc, char *data, int len, double value);
void PX_put_data_long(pxdoc_t *pxdoc, char *data, int len, int value);
void PX_put_data_short(pxdoc_t *pxdoc, char *data, int len, short int value);
void PX_put_data_byte(pxdoc_t *pxdoc, char *data, int len, char value);
void PX_put_data_bcd(pxdoc_t *pxdoc, char *data, int len, char *value);
int PX_put_data_blob(pxdoc_t *pxdoc, char *data, int len, char *value, int valuelen);
void PX_SdnToGregorian(long int sdn, int *pYear, int *pMonth, int *pDay);
long int PX_GregorianToSdn(int year, int month, int day);
pxval_t* PX_make_time(pxdoc_t *pxdoc, int hour, int minute, int second);
pxval_t* PX_make_date(pxdoc_t *pxdoc, int year, int month, int day);
pxval_t* PX_make_timestamp(pxdoc_t *pxdoc, int year, int month, int day, int hour, int minute, int second);
char * PX_timestamp2string(pxdoc_t *pxdoc, double value, const char *format);
char * PX_time2string(pxdoc_t *pxdoc, long value, const char *format);
char * PX_date2string(pxdoc_t *pxdoc, long value, const char *format);
char * PX_strdup(pxdoc_t *pxdoc, const char *str);
        -}

-- These are functions I'm ignoring for now

{-

PXLIB_API int PXLIB_CALL
PX_open_fp(pxdoc_t *pxdoc, FILE *fp);

PXLIB_API int PXLIB_CALL
PX_create_fp(pxdoc_t *pxdoc, pxfield_t *pxf, int numfields, FILE *fp, int type);

PXLIB_API void* PXLIB_CALL
PX_get_opaque(pxdoc_t *pxdoc);

int PX_set_blob_file(pxdoc_t *pxdoc, const char *filename);
int PX_set_blob_fp(pxdoc_t *pxdoc, FILE *fp);
int PX_has_blob_file(pxdoc_t *pxdoc);
pxblob_t* PX_new_blob(pxdoc_t *pxdoc);
int PX_open_blob_fp(pxblob_t *pxdoc, FILE *fp);
int PX_open_blob_file(pxblob_t *pxdoc, const char *filename);
int PX_create_blob_fp(pxblob_t *pxdoc, FILE *fp);
int PX_create_blob_file(pxblob_t *pxblob, const char *filename);
void PX_close_blob(pxblob_t *pxdoc);
void PX_delete_blob(pxblob_t *pxblob);
char* PX_read_blobdata(pxblob_t *pxblob, const char *data, int len, int *mod, int *blobsize);
char* PX_read_graphicdata(pxblob_t *pxblob, const char *data, int len, int *mod, int *blobsize);
char* PX_read_grahicdata(pxblob_t *pxblob, const char *data, int len, int *mod, int *blobsize);

int PX_write_primary_index(pxdoc_t *pxdoc, pxdoc_t *pxindex);
int PX_read_primary_index(pxdoc_t *pindex);
int PX_add_primary_index(pxdoc_t *pxdoc, pxdoc_t *pindex);
-}

-- Deprecated
{-
char * PX_get_record(pxdoc_t *pxdoc, int recno, char *data);
char * PX_get_record2(pxdoc_t *pxdoc, int recno, char *data, int *deleted, pxdatablockinfo_t *pxdbinfo);
int PX_get_data_alpha(pxdoc_t *pxdoc, char *data, int len, char **value);
int PX_get_data_bytes(pxdoc_t *pxdoc, char *data, int len, char **value);
int PX_get_data_double(pxdoc_t *pxdoc, char *data, int len, double *value);
int PX_get_data_long(pxdoc_t *pxdoc, char *data, int len, long *value);
int PX_get_data_short(pxdoc_t *pxdoc, char *data, int len, short int *value);
int PX_get_data_byte(pxdoc_t *pxdoc, char *data, int len, char *value);
int PX_get_data_bcd(pxdoc_t *pxdoc, unsigned char *data, int len, char **value);
int PX_get_data_blob(pxdoc_t *pxdoc, const char *data, int len, int *mod, int *blobsize, char **value);
int PX_get_data_graphic(pxdoc_t *pxdoc, const char *data, int len, int *mod, int *blobsize, char **value);
-}


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
