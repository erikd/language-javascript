#!/bin/sh

# UnicodeDigit
#       any character in the Unicode category “Decimal number (Nd)”

wget -c 'http://www.fileformat.info/info/unicode/category/Nd/list.htm?mode=print' -O uc-nd.htm

grep --no-filename -o -E "U\+[0-9a-fA-F]+" uc-nd.htm | sort > list-nd.txt
