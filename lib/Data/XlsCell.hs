-- |
-- Module      : Data.XlsCell
-- Copyright   : (c) 2022 Olaf.Klinke
--
-- License     : BSD-style
-- Maintainer  : olaf.klinke@phymetric.de
-- Stability   : experimental
-- Portability : GHC
--
-- Static Excel cell values
--
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
module Data.XlsCell (CellF(..),Cell,cellToString) where
import Data.String (IsString(..))
import Text.Printf (printf)

-- | extensible 'Cell' type
data CellF o = NumericalCell Double
    | TextCell String
    | BoolCell Bool 
    | OtherCell o
    deriving (Functor)
instance IsString (CellF o) where
    fromString = TextCell

-- | static 'Cell's in Excel files can hold 
-- numbers, test or booleans. 
type Cell = CellF ()

-- | convert to 'String'. Not the inverse of 'fromString'!
cellToString :: Cell -> String
cellToString (NumericalCell d) = printf "%.15g" d
cellToString (TextCell txt) = txt
cellToString (BoolCell b) = if b then "True" else "False"
cellToString (OtherCell _) = ""
