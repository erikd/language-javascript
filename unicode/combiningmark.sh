#!/bin/sh

# UnicodeCombiningMark
#       any character in the Unicode categories “Non-spacing mark (Mn)” or “Combining spacing mark (Mc)”

wget -c 'http://www.fileformat.info/info/unicode/category/Mn/list.htm?mode=print' -O uc-mn.htm
wget -c 'http://www.fileformat.info/info/unicode/category/Mc/list.htm?mode=print' -O uc-mc.htm

grep --no-filename -o -E "U\+[0-9a-fA-F]+" uc-m*.htm | sort > list-cm.txt
