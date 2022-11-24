# Bookmark diff

A simple tool writen in chicken scheme for extracting new bookmarks from two html bookmark files exported from Firefox/Chrome.

## Build from source
To build it, you need to install [!https://call-cc.org](chicken), then run the following commands
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
./bookmarkdiff -a bookmark1.html -b bookmark2.html -d /path/you/want/it/to/be
```
