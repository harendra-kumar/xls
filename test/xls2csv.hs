#!/usr/bin/env stack
-- stack runhaskell --package getopt-generics

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    (intercalate)
import           Data.Xls
import           WithCli

sink :: MonadResource m => Sink [String] m ()
sink = CL.mapM_ $ liftIO . putStrLn . intercalate ","

main = withCli run

run :: String -> IO ()
run file = runResourceT $ decodeXLS file $$ sink
