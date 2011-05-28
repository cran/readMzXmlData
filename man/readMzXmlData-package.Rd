\encoding{utf8}
\name{readMzXmlData-package}
\alias{readMzXmlData-package}
\docType{package}
\title{The readMzXmlData Package}

\description{
The package reads mass spectrometry data in mzXML format. \cr
The package extracts only necessary functions of Jarek Tuszynski's
(\email{jaroslaw.w.tuszynski@saic.com}) \pkg{caMassClass} and \pkg{caTools}
to avoid a lot of dependencies. \cr
This follows the UNIX philosophy to provide one package which do one thing and
do it well. Also I want to try to keep it small and simple.
}
\details{
\tabular{ll}{                                                                                                                                     
Package: \tab readMzXmlData\cr                                                                                                                   
Type: \tab Package\cr                                                                                                                             
Version: \tab 1.3\cr
Date: \tab 2011-05-28\cr
License: \tab GPL(version 3 or later)\cr                                                                                                          
}    

Main functions:

\code{\link{readMzXmlFile}}: Reads mass spectrometry data in mzXML format.

\code{\link{readMzXmlDir}}: Reads recursively mass spectrometry data in mzXML
format in a specific directory.

\code{\link{mqReadMzXml}}: Reads mass spectrometry data into MALDIquant.
}
\author{
Jarek Tuszynski (SAIC) \email{jaroslaw.w.tuszynski@saic.com} (Author of
\code{\link{.read.mzXML}}), \cr
Sebastian Gibb

Maintainer: Sebastian Gibb \email{mail@sebastiangibb.de}}
\references{
See website: \url{http://www.strimmerlab.org/software/maldiquant/}
}
\keyword{IO}
\seealso{
    \code{\link{readMzXmlDir}}, 
    \code{\link{readMzXmlFile}},
    \code{\link{.read.mzXML}}
}
