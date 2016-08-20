## Haskell xls Parsing
This is a Haskell library to parse Microsoft Excel spreadsheet files. It parses
the xls file format (extension `.xls`) more specifically known as
`BIFF/Excel 97-2004`.

Could be useful for mining data from old Microsoft Excel spreadsheets.

## API
Note: This is not a released package. The API is not stable and might
change freely in future updates.

It provides a Conduit based streaming API. See the haddock documentation
for the API details.

It also provides a utility `xls2csv` to convert xls spreadsheets to csv. The
utility also serves as an example of how to use the library.

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
I wrote this for a specific use case to begin with. I am not actively focused
on this but if you would like to have something changed or added please go
ahead, raise an issue or send a pull request.
