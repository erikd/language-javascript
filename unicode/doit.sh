#!/bin/sh

# Identifier characters
# UnicodeLetter
#       any character in the Unicode categories “Uppercase letter (Lu)”, “Lowercase letter (Ll)”, 
#       “Titlecase letter (Lt)”, “Modifier letter (Lm)”, “Other letter (Lo)”, or “Letter number (Nl)”.

wget -c 'http://www.fileformat.info/info/unicode/category/Lu/list.htm?mode=print' -O uc-lu.htm
wget -c 'http://www.fileformat.info/info/unicode/category/Ll/list.htm?mode=print' -O uc-ll.htm
wget -c 'http://www.fileformat.info/info/unicode/category/Lt/list.htm?mode=print' -O uc-lt.htm
wget -c 'http://www.fileformat.info/info/unicode/category/Lm/list.htm?mode=print' -O uc-lm.htm
wget -c 'http://www.fileformat.info/info/unicode/category/Lo/list.htm?mode=print' -O uc-lo.htm
wget -c 'http://www.fileformat.info/info/unicode/category/Nl/list.htm?mode=print' -O uc-nl.htm

grep --no-filename -o -E "U\+[0-9a-fA-F]+" uc-*.htm | sort > list.txt
