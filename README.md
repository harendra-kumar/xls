## Haskell xls Parsing
This is a Haskell library to parse Microsoft Excel spreadsheet files. It parses
the xls file format (extension `.xls`) more specifically known as
`BIFF/Excel 97-2004`.

## API
Note: This is not a released package. The API is not stable and is likely to
change freely in future updates.

It provides a Conduit based streaming API. See the haddock documentation
for the API details.

It also provides a utility `xls2csv` to convert xls spreadsheets to csv. The
utility also serves as an example of how to use the library.

## libxls
The library is based on the C library libxls, see
[sourceforge](https://sourceforge.net/projects/libxls/) or
[github](https://github.com/svn2github/libxls).

## Contributing
I wrote this for a specific use case to begin with. I am not actively focused
on this but if you would like to have something changed or added please go
ahead, raise an issue or send a pull request.
