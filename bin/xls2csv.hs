#!/usr/bin/env stack
-- stack --resolver lts runhaskell --package getopt-generics

import Data.List (intercalate)
import Data.Xls (decodeXlsIO)
import WithCli (withCli)

-- TODO need to escape the separator and the escaping quotes themselves

xlsToCSV :: String -> IO ()
xlsToCSV file = do
    worksheets <- decodeXlsIO file
    mapM_ (mapM_ (putStrLn . intercalate ",")) worksheets

main :: IO ()
main = withCli xlsToCSV
