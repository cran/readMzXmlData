\name{readMzXmlFile}
\alias{readMzXmlFile}

\title{
Reads mass spectrometry data in mzXML format.
}
\description{
Reads mass spectrometry data in mzXML format defined in
\url{http://tools.proteomecenter.org/mzXMLschema.php}. 
}
\usage{
readMzXmlFile(mzXmlFile, removeMetaData=FALSE, verbose=FALSE)
}
\arguments{
    \item{mzXmlFile}{
        Path to \emph{mzXML} file which should be read.
}
    \item{removeMetaData}{
        logical. To save memory metadata could be deleted.
}
    \item{verbose}{
        logical. Print verbose messages?
}
}
\value{
A list of spectra and metadata.

    \item{spectrum$mass}{
        A vector of calculated mass.
}
    \item{spectrum$intensity}{
        A vector of intensity values. 
}
    \item{metaData}{
        A list of metaData depending on read spectrum.
}
}
\references{ 
  Definition of \code{mzXML} format:
  \url{http://tools.proteomecenter.org/mzXMLschema.php}
  
  Documentation of \code{mzXML} format:
  \url{http://sashimi.sourceforge.net/schema_revision/mzXML_2.1/Doc/mzXML_2.1_tutorial.pdf}
}
\author{
Sebastian Gibb
}
\seealso{
  \code{\link{readMzXmlDir}}
}
\examples{
## load library
library("readMzXmlData");

## get examples directory
exampleDirectory <- system.file("Examples", package="readMzXmlData");

## read example spectrum
spec <- readMzXmlFile(file.path(exampleDirectory, "A1-0_A1.mzXML"));

## print metaData
print(spec$metaData);

## plot spectrum
plot(spec$spectrum$mass, spec$spectrum$intensity, type="l", col="red");

}
\keyword{IO}
