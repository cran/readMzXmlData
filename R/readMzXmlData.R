## $Id:readMzXmlData.R 381 2011-02-15 15:58:49Z sgibb $
##
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
    if (verbose)
        message("Look for spectra in ", sQuote(mzXmlDir), " ...");

    if ((!file.exists(mzXmlDir)) || (!file.info(mzXmlDir)$isdir)) {
        warning("Directory ", sQuote(mzXmlDir), " doesn't exists or is no
                directory!");
        return(NA);
    }

    ## look for fid files (alphabetical sort)
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
        warning("Directory doesn't contain any ", fileExtension, " file.");
        return(NA);
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
            mzXmlData <- append(mzXmlData, spectra); 
        }
    }

    if (!removeMetaData & rewriteNames) {
        ## rewrite names
        if (verbose)
            message("rewrite names ...");

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
    ## try to get absolute file path
    mzXmlFile <- normalizePath(mzXmlFile);

    if (verbose)
        message("Reading spectrum from ", sQuote(mzXmlFile), " ...");
  
    if (!file.exists(mzXmlFile)) {
        warning("File ", sQuote(mzXmlFile), " doesn't exists!");
        return(NA);
    }

    if (file.info(mzXmlFile)$isdir) {
        warning("Not a mzXML file! ", sQuote(mzXmlFile), " is a directory.");
        return(NA);
    }

    ## read file
    s <- .read.mzXML(mzXmlFile);

    spectra <- lapply(s$scan, function(x, globalS=s) {
            scan <- list()
            scan$spectrum <- list();
            scan$spectrum$mass <- x$mass;
            scan$spectrum$intensity <- x$peaks;
            if (!removeMetaData) {
                scan$metaData <- .globalMetaData(globalS);
                scan$metaData <- append(scan$metaData, .scanMetaData(x));
                scan$metaData$file <- mzXmlFile;
            }
            return(scan);
        });

    return(spectra);
}

## function .globalMetaData
##  convert metadata from read.mzXML format
##  works on global metadata (the same for all scans of the same mzXML file)
##
## params:
##  spec: spectrum list (direct output of read.mzXML) 
##
## returns:
##  a list with metadata
##
.globalMetaData <- function(spec) {
    keys <- c("msInstrument", "parentFile", "dataProcessing", "separation",
        "spotting", "indexOffset");

    metaData <- list();
    for (i in keys) {
        if (length(spec[[i]]) > 0) {
            metaData[[i]] <- spec[[i]];
        }
    }

    return(metaData);
}

## function .scanMetaData
##  convert metadata from read.mzXML format
##  works on scan specific metadata
##
## params:
##  scan: scan list (direct output of read.mzXML(...)$scan[[1]])
##
## returns:
##  a list with metadata
##
.scanMetaData <- function(scan) {
    keys <- c("num", "parentNum", "msLevel", "header", "maldi",
        "experiment", "scanOrigin", "precursorMz");

    metaData <- list();
    for (i in keys) {
        if (length(scan[[i]]) > 0) {
            metaData[[i]] <- scan[[i]];
        }
    }

    return(metaData);
}

## EOF
