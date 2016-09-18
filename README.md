## Haskell xls Parsing

[![Build Status](https://travis-ci.org/harendra-kumar/xls.svg?branch=master)](https://travis-ci.org/harendra-kumar/xls)
[![Build status](https://ci.appveyor.com/api/projects/status/nwknaf0gw1p9vqnv?svg=true)](https://ci.appveyor.com/project/harendra-kumar/xls)

`xls` is a Haskell library to parse Microsoft Excel spreadsheet files. It
parses the xls file format (extension `.xls`) more specifically known as
`BIFF/Excel 97-2004`.

It can be useful for mining data from old Microsoft Excel spreadsheets.

## API
Use `decodeXls` to get a streaming Conduit. For example to convert an
xls file to comma separated csv:

```haskell
xlsToCSV :: String -> IO ()
xlsToCSV file =
      runResourceT
    $ decodeXls file
    $$ CL.mapM_ (liftIO . putStrLn . intercalate ",")
```

An `xls2csv` utility is shipped with the package.
See the [haddock
documentation](https://rawgit.com/harendra-kumar/xls/master/doc/index.html)
for the API details.

## Under the hood
The library is based on the C library libxls, see
[sourceforge](https://sourceforge.net/projects/libxls/) or
[github](https://github.com/svn2github/libxls).

## Related Stuff

### Haskell
xlsx format:
* https://hackage.haskell.org/package/xlsior Streaming Excel (xslx) file generation and parsing
* https://hackage.haskell.org/package/xlsx Excel xslx file parser/writer
* https://hackage.haskell.org/package/xlsx-templater create xlsx data files from xlsx templates
* https://hackage.haskell.org/package/hs-excelx read-only access to Excel 2007 and 2010 documents in XLSX format

Others:
* https://hackage.haskell.org/package/SpreadsheetML Write support for Excel's SpreadsheetML format

### C
* https://sourceforge.net/projects/libxls/
* https://www.gaia-gis.it/fossil/freexl/index Available as debian package too.
* https://gitlab.com/orcus/orcus C++ XML workbooks

### Java
* http://poi.apache.org/ Jakarta POI

### Python
* http://pandas.pydata.org/pandas-docs/stable/generated/pandas.read_excel.html

### JavaScript
* https://www.npmjs.com/package/excel-parser
* https://www.npmjs.com/package/xlsx

### Perl
* ParseExcel

## Contributing
Welcome! If you would like to have something changed or added go ahead,
raise an issue or send a pull request.
