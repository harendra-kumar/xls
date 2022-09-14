-- |
-- Module      : Data.Xls
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parse Microsoft excel spreadsheet xls file (format BIFF/Excel 97-2004).
--
{-# OPTIONS_GHC -pgmP gcc -optP -E -optP -undef -optP -std=c89 #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
#if __GLASGOW_HASKELL__ < 7100
{-# LANGUAGE DeriveDataTypeable #-}
#endif

module Data.Xls
    ( decodeXlsIO
    , decodeXlsByteString
    , decodeXlsByteString'
    , decodeXls
    , XlsException(..)
    , XLSError(..)
    )
where

import           Control.Exception (Exception, throwIO, bracket, catch)
import           Control.Monad.IO.Class
import           Control.Monad (void)
import           Control.Monad.Trans.Resource
import           Data.Conduit hiding (Conduit, Sink, Source)
import           Data.Data
import           Data.Int
import           Data.Word (Word32)
import           Data.Maybe (catMaybes, fromJust, isJust)
import           Data.ByteString (hPut)
import           Data.ByteString.Internal (ByteString(..))
import           Data.XlsCell (CellF(..),Cell,cellToString)
import           Foreign.C
import           Foreign.Ptr
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Storable (Storable(..))
import           Foreign.Marshal.Alloc (malloc)
import           System.IO.Temp (withSystemTempFile)

#define CCALL(name,signature) \
foreign import ccall unsafe #name \
    c_##name :: signature

-- Workbook accessor functions
data XLSWorkbookStruct
-- | An enum returned by libxls. See libxls\/include\/xls.h
data XLSError = LIBXLS_OK 
    | LIBXLS_ERROR_OPEN 
    | LIBXLS_ERROR_SEEK 
    | LIBXLS_ERROR_READ 
    | LIBXLS_ERROR_PARSE 
    | LIBXLS_ERROR_MALLOC
    deriving (Show,Eq,Enum)
instance Storable XLSError where
    sizeOf = const (sizeOf (0 :: Word32)) -- TODO: sizeof enum is compiler and architecture dependent!
    alignment = sizeOf -- okay for simple Storables
    peek = fmap (toEnum . (fromIntegral :: Word32 -> Int)) . peek . castPtr
    poke ptr e = poke (castPtr ptr) ((fromIntegral :: Int -> Word32).fromEnum $ e)
type XLSWorkbook = Ptr XLSWorkbookStruct
type XLSErrorT = Ptr XLSError
type CBuffer = Ptr CUChar

-- | Recall that
-- @
-- ByteString ~ (ForeignPtr Char8,Int)
--     CUChar ~ Word8
--      CSize ~ Word64
-- @
--
-- So we need to marshal
-- 
-- @
-- (ForeignPtr Char8) -> Ptr CUChar
-- Int -> CSize
-- @
toCBuffer :: ByteString -> IO (CBuffer,CSize)
toCBuffer (PS fPtr offset ilen) = do
    withForeignPtr fPtr $ \ptrData -> do
        return (plusPtr (castPtr ptrData) offset,CSize (fromIntegral ilen))

CCALL(xls_open,          CString -> CString -> IO XLSWorkbook)
CCALL(xls_open_buffer,   CBuffer -> CSize -> CString -> XLSErrorT -> IO XLSWorkbook)
CCALL(xls_wb_sheetcount, XLSWorkbook -> IO CInt -- Int32)
CCALL(xls_close_WB,      XLSWorkbook -> IO ())

-- Worksheet accessor functions
data XLSWorksheetStruct
type XLSWorksheet = Ptr XLSWorksheetStruct

CCALL(xls_getWorkSheet, XLSWorkbook -> CInt -> IO XLSWorksheet)

CCALL(xls_parseWorkSheet, XLSWorksheet -> IO ())
CCALL(xls_ws_rowcount,    XLSWorksheet -> IO Int16 -- Int16)
CCALL(xls_ws_colcount,    XLSWorksheet -> IO Int16 -- Int16)
CCALL(xls_close_WS,       XLSWorksheet -> IO ())

-- Cell accessor functions
data XLSCellStruct
type XLSCell = Ptr XLSCellStruct

CCALL(xls_cell, XLSWorksheet -> Int16 -> Int16 -> IO XLSCell)

CCALL(xls_cell_type,            XLSCell -> IO Int16 -- Int16)
CCALL(xls_cell_strval,          XLSCell -> IO CString)
CCALL(xls_cell_formulatype,     XLSCell -> IO Int32 -- Int32)
CCALL(xls_cell_numval,          XLSCell -> IO CDouble)
-- CCALL(xls_cell_colspan,         XLSCell -> IO Int16 -- Int16)
-- CCALL(xls_cell_rowspan,         XLSCell -> IO Int16 -- Int16)
CCALL(xls_cell_hidden,          XLSCell -> IO Int8 -- Int8)

data XlsException =
      XlsFileNotFound String
    | XlsParseError String
    deriving (Show, Typeable)

instance Exception XlsException

exceptionLeft :: XlsException -> Either XLSError a
exceptionLeft (XlsFileNotFound _) = Left LIBXLS_ERROR_OPEN
exceptionLeft (XlsParseError   _) = Left LIBXLS_ERROR_PARSE

catchXls :: IO a -> IO (Either XLSError a)
catchXls = flip catch (return.exceptionLeft) . fmap Right 

-- | Parse a Microsoft excel xls workbook file into a Conduit yielding
-- rows in a worksheet. Each row represented by a list of Strings, each String
-- representing an individual cell.
--
-- Important Note: This API concatenates rows from all worksheets into a single
-- stream. Please use the non-streaming 'decodeXlsIO' API to get individual
-- worksheets.
--
-- Throws 'XlsException'
--
decodeXls :: MonadResource m => FilePath -> ConduitM i [String] m ()
decodeXls file =
    bracketP alloc cleanup decodeWorkSheets
    where
        alloc = do
            file' <- newCString file
            pWB <- newCString "UTF-8" >>= c_xls_open file'
            if pWB == nullPtr then
                throwIO $ XlsFileNotFound
                        $ "XLS file " ++ file ++ " not found."
            else
                return pWB

        cleanup = c_xls_close_WB

        decodeWorkSheets pWB = do
            count <- liftIO $ c_xls_wb_sheetcount pWB
            mapM_ (decodeOneWorkSheet file pWB) [0 .. count - 1]

-- | A work-around via temporary files and 'decodeXlsIO', 
-- since this lib is lacking a pure function to decode the contents 
-- of an XLS file. 
-- Due to Erik Rybakken. 
decodeXlsByteString :: ByteString -> IO [[[Cell]]]
decodeXlsByteString content = withSystemTempFile "decodeXlsByteString"
    $ \filePath h -> do
        hPut h content
        decodeXlsIO filePath

-- | Experimental: This function uses the @xls_open_buffer@ function of libxls.
decodeXlsByteString' :: ByteString -> IO (Either XLSError [[[Cell]]])
decodeXlsByteString' bs = do
    (buf,buflen) <- toCBuffer bs
    enc <- newCString "UTF-8"
    outError <- malloc
    wb <- c_xls_open_buffer buf buflen enc outError
    e <- peek outError
    case e of
        LIBXLS_OK -> decodeXLSWorkbook Nothing wb
        _         -> return (Left e) 

-- | Parse a Microsoft excel xls workbook file into a list of worksheets, each
-- worksheet consists of a list of rows and each row consists of a list of
-- cells.  Cells are plain 'String'.
--
-- Throws 'XlsException'
--
decodeXlsIO
    :: FilePath
    -> IO [[[Cell]]]
decodeXlsIO file = do
    file' <- newCString file
    pWB <- newCString "UTF-8" >>= c_xls_open file'
    parseResult <- decodeXLSWorkbook (Just file) pWB
    case parseResult of
        Right results -> return results
        Left  e -> case e of
            LIBXLS_ERROR_OPEN -> throwIO $ XlsFileNotFound $ 
                "XLS file " ++ file ++ " not found."
            _                 -> throwIO $ XlsParseError $ 
                "XLS file " ++ file ++ " could not be parsed."

-- helper function for decoding both file and buffer
decodeXLSWorkbook :: Maybe FilePath -> XLSWorkbook -> IO (Either XLSError [[[Cell]]])
decodeXLSWorkbook mFile pWB = if pWB == nullPtr
    then return (Left LIBXLS_ERROR_OPEN)
    else catchXls $ do 
        count <- liftIO $ c_xls_wb_sheetcount pWB
        results <- mapM (decodeOneWorkSheetIO (maybe "buffer" id mFile) pWB) [0 .. count - 1]
        void $ c_xls_close_WB pWB
        return results
    

decodeOneWorkSheet
    :: MonadResource m
    => FilePath -> XLSWorkbook -> CInt -> ConduitM i [String] m ()
decodeOneWorkSheet file pWB index =
    bracketP alloc cleanup decodeWS
    where
        alloc = do
            pWS <- c_xls_getWorkSheet pWB index
            if pWS == nullPtr then
                throwIO $ XlsParseError
                        $ "XLS file " ++ file ++ " could not be parsed."
            else do
              c_xls_parseWorkSheet pWS
              return pWS

        cleanup = c_xls_close_WS

        decodeWS = decodeRows

decodeOneWorkSheetIO
    :: FilePath
    -> XLSWorkbook
    -> CInt
    -> IO [[Cell]]
decodeOneWorkSheetIO file pWB index =
    bracket alloc cleanup decodeRowsIO
    where
        alloc = do
            pWS <- c_xls_getWorkSheet pWB index
            if pWS == nullPtr then
                throwIO $ XlsParseError
                        $ "XLS file "
                        ++ file
                        ++ " could not be parsed."
            else do
              c_xls_parseWorkSheet pWS
              return pWS
        cleanup = c_xls_close_WS

decodeRows :: MonadResource m => XLSWorksheet -> ConduitM i [String] m ()
decodeRows pWS = do
    rows <- liftIO $ c_xls_ws_rowcount pWS
    cols <- liftIO $ c_xls_ws_colcount pWS
    mapM_ (decodeOneRow pWS cols) [r | r <- [0 .. rows - 1]]

decodeRowsIO
    :: XLSWorksheet
    -> IO [[Cell]]
decodeRowsIO pWS = do
    rows <- c_xls_ws_rowcount pWS
    cols <- c_xls_ws_colcount pWS
    mapM (decodeOneRowIO pWS cols) [r | r <- [0 .. rows - 1]]

decodeOneRow
    :: MonadResource m
    => XLSWorksheet -> Int16 -> Int16 -> ConduitM i [String] m ()
decodeOneRow pWS cols rowindex =
    mapM (liftIO . (c_xls_cell pWS rowindex)) [0 .. cols - 1]
        >>= mapM (liftIO . decodeOneCell)
        >>= yield . catMaybes

decodeOneRowIO
    :: XLSWorksheet
    -> Int16
    -> Int16
    -> IO [Cell]
decodeOneRowIO pWS cols rowindex =
    mapM (c_xls_cell pWS rowindex) [0 .. cols - 1]
        >>= mapM decodeOneCell'

data CellType = Numerical | Formula | Str | Other

decodeOneCell :: XLSCell -> IO (Maybe String)
decodeOneCell = fmap maybeString . decodeOneCell' where
    maybeString (OtherCell _) = Nothing
    maybeString c = Just (cellToString c)

decodeOneCell' :: XLSCell -> IO Cell
decodeOneCell' cellPtr = do
    nil <- isNullCell cellPtr
    if nil then
        return (OtherCell ())
    else cellValue cellPtr

    where
        emptyCell = OtherCell ()
        isNullCell ptr =
            if ptr == nullPtr then
                return True
            else do
                hidden <- c_xls_cell_hidden ptr
                if hidden /= 0 then
                    return True
                else
                    return False

        cellValue ptr = do
            typ     <- c_xls_cell_type ptr
            numval  <- c_xls_cell_numval ptr
            ftype   <- c_xls_cell_formulatype ptr
            --rowspan <- c_xls_cell_rowspan ptr
            --colspan <- c_xls_cell_colspan ptr
            pStr    <- c_xls_cell_strval ptr
            strval  <-
                if pStr /= nullPtr then
                    peekCString pStr >>= return . Just
                else
                    return Nothing

            return $ case cellType typ ftype strval of
                Numerical   -> let (CDouble d) = numval in NumericalCell d
                Formula     -> decodeFormula strval numval
                Str         -> (TextCell . fromJust) strval
                Other       -> emptyCell -- we don't decode anything else

        decodeFormula str numval =
            case str of
                Just "bool"  -> outputBool numval
                Just "error" -> TextCell "*error*"
                Just x       -> TextCell x
                Nothing      -> emptyCell -- is it possible?

        outputBool d = BoolCell (if d == 0 then False else True)

        cellType t ftype strval =
            if t == 0x27e || t == 0x0BD || t == 0x203 then
                Numerical
            else if t == 0x06 then
                if ftype == 0 then
                    Numerical
                else
                    Formula
            else if isJust strval then
                Str
            else
                Other
