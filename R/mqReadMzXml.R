## $Id:readMzXmlData.R 381 2011-02-15 15:58:49Z sgibb $
##
## Copyright 2010-2011 Sebastian Gibb
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

## function mqReadMzXml 
##  imports mzXML files into MALDIquant MassSpectrum class 
##
##  WARNING: this is a recursive function!
##
## params:
##  path: path to root dir of mzXML files/or a single mzXML file e.g. data/
##
## returns:
##  a MALDIquant MassSpectrum 
##
mqReadMzXml <- function(path, ...) {

    if (!file.exists(path)) {
        stop("Path ", sQuote(path), " doesn't exists!");
    }

    if (!require("MALDIquant")) {
        stop("Could not load package ", sQuote("MALDIquant"), ".");
    }

    if (!file.info(path)$isdir) {
        s <- readMzXmlFile(mzXmlFile=path, ...);

        ## make list structure equal for single spectrum mzXML files
        if (!is.null(s$spectrum$mass)) {
          l <- list();
          l[[1]] <- s;
          s <- l;
        }
    } else {
        s <- readMzXmlDir(mzXmlDir=path, ...);
    }
    s <- lapply(s, function(x) {
                return(createMassSpectrum(mass=x$spectrum$mass,
                                          intensity=x$spectrum$intensity,
                                          metaData=x$metaData)); });
    if (length(s) == 1) {
        return(s[[1]]);
    } else {
        return(s);
    }
}

