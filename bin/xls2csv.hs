#!/usr/bin/env stack
-- stack --resolver lts runhaskell --package getopt-generics

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    (intercalate)
import           Data.Xls
import           WithCli

-- TODO need to escape the separator and the escaping quotes themselves

xlsToCSV :: String -> IO ()
xlsToCSV file =
      runResourceT
    $ runConduit
    $ decodeXls file
    .| CL.mapM_ (liftIO . putStrLn . intercalate ",")

main = withCli xlsToCSV
