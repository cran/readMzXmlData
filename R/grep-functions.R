## Copyright 2011 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of readMzXmlData for R and related languages.
##
## readMzXmlData is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## readMzXmlData is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with readMzXmlData. If not, see <http://www.gnu.org/licenses/>

## function .grepSubString 
##  grep a regexp in a substring
##
## params:
##  pattern: regexp pattern
##  x: text
##
## returns:
##  matched substring
##
.grepSubString <- function(pattern, x) {
    rx <- regexpr(pattern=pattern, text=x);
    str <- substr(x=x, start=rx, stop=(rx + (attr(rx, "match.length") - 1) ));
    return(str);
}

## function .grepNumber/.grepDouble
##  grep a number/double in a string 
##
## params:
##  x: text
##
## returns:
##  matched number string/double
##
.grepNumber <- function(x) {
    return(.grepSubString(pattern="[0-9]+\\.?[0-9]*", x=x));
}

.grepDouble <- function(x) {
    return(as.double(.grepNumber(x)));
}
