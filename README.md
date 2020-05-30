## Haskell xls Parsing

[![Hackage](https://img.shields.io/hackage/v/xls.svg?style=flat)](https://hackage.haskell.org/package/xls)
[![Build Status](https://travis-ci.org/harendra-kumar/xls.svg?branch=master)](https://travis-ci.org/harendra-kumar/xls)
[![Build status](https://ci.appveyor.com/api/projects/status/nwknaf0gw1p9vqnv?svg=true)](https://ci.appveyor.com/project/harendra-kumar/xls)

`xls` is a Haskell library to parse Microsoft Excel spreadsheet files. It
parses the xls file format (extension `.xls`) more specifically known as
`BIFF/Excel 97-2004`.

It can be useful for mining data from old Microsoft Excel spreadsheets.

## API
Use `decodeXlsIO` to get a list of all worksheets. For example to convert all
worksheets in an xls file to comma separated csv:

```haskell
import Data.List (intercalate)
import Data.Xls (decodeXlsIO)

xlsToCSV :: String -> IO ()
xlsToCSV file = do
    worksheets <- decodeXlsIO file
    mapM_ (mapM_ (putStrLn . intercalate ",")) worksheets
```

An `xls2csv` utility is shipped with the package.
See the haddock documentation for API details.

## Under the hood
The library is based on the C library 
[libxls](https://github.com/libxls/libxls).

## See Also

* [xlsior](https://hackage.haskell.org/package/xlsior): Streaming Excel (xslx) file generation and parsing
* [xlsx](https://hackage.haskell.org/package/xlsx): Excel xslx file parser/writer

## Contributing
Welcome! If you would like to have something changed or added go ahead,
raise an issue or send a pull request.
