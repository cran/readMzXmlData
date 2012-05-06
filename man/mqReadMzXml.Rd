\name{mqReadMzXml}
\alias{mqReadMzXml}

\title{
Reads mass spectrometry data into MALDIquant.
}
\description{
Reads all mass spectrometry data in mzXML format in
a specified path into \code{\link[MALDIquant]{MALDIquant-package}} 
\code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\usage{
mqReadMzXml(path, \dots)
}
\arguments{
    \item{path}{
        Path to \emph{directory} or a single \emph{mzXML} file.
}
    \item{\dots}{
        arguments to be passed to
        \code{\link[readMzXmlData]{readMzXmlDir}} or
        \code{\link[readMzXmlData]{readMzXmlFile}} 
}
}
\details{
See \code{\link[readMzXmlData]{readMzXmlDir}} or
\code{\link[readMzXmlData]{readMzXmlFile}}.
}
\value{
    Returns a list of \code{\link[MALDIquant]{MassSpectrum-class}} objects.
}
\references{                                                                                                                                                                                 
See website: \url{http://strimmerlab.org/software/maldiquant/}                                                                                                                               
}  
\author{
Sebastian Gibb
}
\seealso{
\code{\link[readMzXmlData]{readMzXmlDir}},
\code{\link[readMzXmlData]{readMzXmlFile}},
\code{\link[MALDIquant]{MALDIquant-package}},
\code{\link[MALDIquant]{MassSpectrum-class}}
}
\examples{
## load library
library("readMzXmlData");

## get examples directory
exampleDirectory <- system.file("Examples", package="readMzXmlData");

## read example spectra
spec <- mqReadMzXml(exampleDirectory);

## plot spectra
par(mfrow=c(2, 1));
lapply(spec, plot);
par(mfrow=c(1, 1));
}
\keyword{IO}
