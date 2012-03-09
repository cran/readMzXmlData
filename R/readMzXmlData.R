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

## function readMzXmlDir
##  reads all mzXML files in one directory
##
##  WARNING: this is a recursive function!
##
## params:
##  mzXmlDir: path to root dir of mzXML files e.g. data/
##  removeCalibrationScans: default TRUE, don't read spectra from calibration
##                          scans
##  removeMetaData: see readMzXmlFile for details (default: FALSE)
##  rewriteNames: TRUE/FALSE [default: rewriteNames=TRUE]
##  fileExtension: sometimes mzXML called *.xml [default: "mzXML"] 
##  verbose: TRUE/FALSE [default: verbose=FALSE]
##
## returns:
##  a list with metadata and spectra
##
readMzXmlDir <- function(mzXmlDir, removeCalibrationScans=TRUE,
    removeMetaData=FALSE, rewriteNames=TRUE, fileExtension="mzXML",
    verbose=FALSE) {
    if (verbose) {
        message("Look for spectra in ", sQuote(mzXmlDir), " ...");
    }

    if ((!file.exists(mzXmlDir)) || (!file.info(mzXmlDir)$isdir)) {
        stop("Directory ", sQuote(mzXmlDir), " doesn't exists or is no ",
             "directory!");
    }

    ## look for mzXML files (alphabetical sort)
    files <- list.files(path=mzXmlDir, pattern=paste("*.", fileExtension, "$",
            sep=""), recursive=TRUE);

    ## remove calibrations scans?
    if (removeCalibrationScans) {
        calibrationScans <- grep(pattern="[Cc]alibration", x=files, value=TRUE);
        if (length(calibrationScans) > 0) {
        files <- setdiff(files, calibrationScans);
        }
    }

    if (length(files) <= 0) {
        stop("Directory doesn't contain any ", fileExtension, " file.");
    }

    ## generate "path/files"
    files <- sapply(files, function(x) {
            x <- file.path(mzXmlDir, x);
            return(x);
    });

    ## read mzXML files
    mzXmlData <- list();
    for (i in seq(along=files)) {
        mzXmlFile <- .readMzXmlFile(mzXmlFile=files[i],
            removeMetaData=removeMetaData, verbose=verbose);
        for (j in seq(along=mzXmlFile)) {
            spectra <- list();
            spectra$spectra <- mzXmlFile[[j]];
            mzXmlData <- c(mzXmlData, spectra); 
        }
    }

    if (!removeMetaData & rewriteNames) {
        ## rewrite names
        if (verbose) {
            message("rewrite names ...");
        }

        names(mzXmlData) <- paste("s", 1:length(mzXmlData), sep="");
    }

    return(mzXmlData);
}

## function readMzXmlFile
##  read a single mzXML file
##  only a wrapper function for .readMzXmlFile
##
## params:
##  mzXmlFile: path to mzXML file e.g. Pankreas_HB_L_061019_A10.mzXML
##  removeMetaData: if TRUE => don't return metadata to save memory 
##                  [default: removeMetaData=FALSE]
##  verbose: TRUE/FALSE [default: verbose=FALSE]
##
## returns:
##  a list with intensity, mass and metadata (if removeMetaData == FALSE)
##
##  spectrum$intensity, spectrum$mass, metaData
##
##  if a mzXML file contains more scans a list would returned
##    [[1]]$spectrum$intensity
##    [[1]]$spectrum$mass
##    [[1]]$metaData
##
readMzXmlFile <- function(mzXmlFile, removeMetaData=FALSE, verbose=FALSE) {
    scans <- .readMzXmlFile(mzXmlFile=mzXmlFile, removeMetaData=removeMetaData,
        verbose=verbose);

    if (length(scans) <= 1) {
        return(scans[[1]]);
    } else {
        return(scans);
    }
}

## function .readMzXmlFile
##  read a single mzXML file
##
## params:
##  mzXmlFile: path to mzXML file e.g. Pankreas_HB_L_061019_A10.mzXML
##  removeMetaData: if TRUE => don't return metadata to save memory 
##                  [default: removeMetaData=FALSE]
##  verbose: TRUE/FALSE [default: verbose=FALSE]
##
## returns:
##  a list with intensity, mass and metadata (if removeMetaData == FALSE)
##
##  spectrum$intensity, spectrum$mass, metaData
##
##  if a mzXML file contains more scans a list would returned
##    [[1]]$spectrum$intensity
##    [[1]]$spectrum$mass
##    [[1]]$metaData
##
.readMzXmlFile <- function(mzXmlFile, removeMetaData=FALSE, verbose=FALSE) {
    if (verbose) {
        message("Reading spectrum from ", sQuote(mzXmlFile), " ...");
    }
  
    if (!file.exists(mzXmlFile)) {
        stop("File ", sQuote(mzXmlFile), " doesn't exists!");
    }

    if (file.info(mzXmlFile)$isdir) {
        stop("Not a mzXML file! ", sQuote(mzXmlFile), " is a directory.");
    }

    ## try to get absolute file path
    mzXmlFile <- normalizePath(mzXmlFile);

    ## read file
    s <- .parseMzXml(file=mzXmlFile, verbose=verbose);

    spectra <- lapply(s$scan, function(x, globalS=s) {
            scan <- list()
            scan$spectrum <- x$spectrum;
            scan$metaData$file <- mzXmlFile;

            if (!removeMetaData) {
                scan$metaData <- c(scan$metaData, globalS$metaData, x$metaData);
            }
            return(scan);
    });

    return(spectra);
}
