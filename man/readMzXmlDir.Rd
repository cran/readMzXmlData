\name{readMzXmlDir}
\alias{readMzXmlDir}

\title{
Reads recursively mass spectrometry data in mzXML format.
}
\description{
Reads recursively all mass spectrometry data in mzXML format in
a specified directory.
}
\usage{
readMzXmlDir(mzXmlDir, removeCalibrationScans=TRUE, 
    removeMetaData=FALSE, rewriteNames=TRUE, fileExtension="mzXML", 
    verbose=FALSE)
}
\arguments{
    \item{mzXmlDir}{
        Path to \emph{directory} which should be read recursively.
}
    \item{removeCalibrationScans}{
        logical. If TRUE all scans in directories called [Cc]alibration will be
        ignored.
}
    \item{removeMetaData}{
        logical. To save memory metadata could be deleted.
}
    \item{rewriteNames}{
        logical. If TRUE all list elements get an unique name from metadata
        otherwise file path is used.
}
    \item{fileExtension}{
        File extension of mzXML formatted file. The directory is only looked for
        \emph{fileExtension} files. In most cases it would be "mzXML" but
        sometimes you have to use "xml".
}
    \item{verbose}{
        logical. Print verbose messages?
}
}
\details{
see \code{\link{readMzXmlFile}}
}
\value{
A list of spectra.

    \item{[[1]]$spectrum$mass}{
        A vector of calculated mass.
}
    \item{[[1]]$spectrum$intensity}{
        A vector of intensity values. 
}
    \item{[[1]]$metaData}{
        A list of metaData depending on read spectrum.
}
}
\author{
Sebastian Gibb
}
\seealso{
    \code{\link{readMzXmlFile}},
    \code{\link{read.mzXML}}
}
\examples{
library(readMzXmlData);

## get examples directory
exampleDirectory <- system.file("Examples", package="readMzXmlData");

## read example spectra
spec <- readMzXmlDir(exampleDirectory);

## plot spectra
plot(spec[[1]]$spectrum$mass, spec[[1]]$spectrum$intensity, type="n");

l <- length(spec);
legendStr <- character(l);
for (i in seq(along=spec)) {
    lines(spec[[i]]$spectrum$mass, spec[[i]]$spectrum$intensity, type="l",
    col=rainbow(l)[i]);
    legendStr[i] <- basename(spec[[i]]$metaData$file);
}

## draw legend
legend(x="topright", legend=legendStr, col=rainbow(l), lwd=1);
}
\keyword{IO}
