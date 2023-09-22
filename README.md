# Bookmark diff

A simple tool writen in [chicken scheme](https://call-cc.org) for extracting new bookmarks from two html bookmark files exported from Firefox/Chrome.

## Build from source
To build it, you need to install [chicken](https://wiki.call-cc.org/platforms), then run the following commands
```{sh}
git clone https://github.com/alxbnct/bookmarkdiff.git
chicken-install -n
./bookmarkdiff
```

## Usage
```{sh}
$ bookmarkdiff --help
Usage:
 -a, --input-file-a=FILE  Input file a
 -b, --input-file-b=FILE  Input file b
 -o, --output-file=FILE   Output file name
 -d, --directory=DIR      Directory for new bookmarks
```

## Example
If you want to put the new bookmarks to a certain directory in the browser, you can use the `-d` flag as follows
```{sh}
./bookmarkdiff -a bookmarks_firefox.html -b bookmarks_chrome_2023_8_24.html  -o diff8.html -d "Bookmarks Menu/Other Bookmarks/Chrome/2023-08-24"
```
