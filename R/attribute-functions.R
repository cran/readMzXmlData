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

## function .attributeToString
##  convert an XML attribute to string 
##
## params:
##  attributes: vector of attributes
##  attributeName: name of attribute to convert
##  required: logical, throw an error if an required attribute is missing
##
## returns:
##  attribute string 
##
.attributeToString <- function(attributes, attributeName, required=FALSE) {
    a <- unname(attributes[attributeName]);
    if (required && is.na(a)) {
        stop("Malformed mzXML: attribute ", sQuote(attributeName), " is missing!")
    } else {
        return(a);
    }
}

## function .attributeToDouble
##  convert an XML attribute to double
##
## params:
##  attributes: vector of attributes
##  attributeName: name of attribute to convert
##  required: logical, throw an error if an required attribute is missing
##
## returns:
##  attribute as double
##
.attributeToDouble <- function(attributes, attributeName, required=FALSE) {
    return(as.double(.attributeToString(attributes=attributes,
                                        attributeName=attributeName,
                                        required=required)));
}

## function .attributeTimeToDouble
##  convert an XML time attribute to double
##
## params:
##  attributes: vector of attributes
##  attributeName: name of attribute to convert
##  required: logical, throw an error if an required attribute is missing
##
## returns:
##  attribute as double
##
.attributeTimeToDouble <- function(attributes, attributeName, required=FALSE) {
    return(as.double(.grepDouble(.attributeToString(attributes=attributes,
                                    attributeName=attributeName,
                                    required=required))));
}
