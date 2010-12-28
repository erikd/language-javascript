#!/bin/sh


# UnicodeConnectorPunctuation
#       any character in the Unicode category “Connector punctuation (Pc)”

wget -c 'http://www.fileformat.info/info/unicode/category/Pc/list.htm?mode=print' -O uc-pc.htm

grep --no-filename -o -E "U\+[0-9a-fA-F]+" uc-pc.htm | sort > list-pc.txt
