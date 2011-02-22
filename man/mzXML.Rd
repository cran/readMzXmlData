% completely taken from caMassClass 1.9 R/mzXML.R
% modification by Sebastian Gibb <mail@sebastiangibb.de>:
% - remove write.mzXML function and its examples
% - remove new.mzXML function
\name{.read.mzXML}
\alias{.read.mzXML}
\title{Read mzXML Files.}
\description{Read protein mass spectra data to/from mzXML files.}
\usage{
  mzXML = .read.mzXML(filename)
}

\arguments{
  \item{filename}{character string with name of the file (connection)}
}

\details{
  This function was taken from \pkg{caMassClass} 1.9 \emph{R/mzXML.R}.
  The main task of \code{.read.mzXML} function is to extract scan data of mzXML
  files. In addition attempt is made to keep all other sections of mzXML file
  as unparsed XML code, so the data can be extracted latter. 
  Those unparsed sections are stored as XML text.
}                                    

\value{
  Function \code{read.mzXML} returns object of type 
  \code{mzXML}, containing: 
  \item{scan}{List of Mass Spectra scans. Each element of the list contain the 
    following elements: 
    \itemize{
      \item \code{peaks} - intensities or peaks of the scan 
      \item \code{mass}  - masses (m/z) corresponding to \code{peaks}. Vectors 
      \code{mass} and \code{peaks} have the same length.
      \item \code{num} - scan number
      \item \code{parentNum} - scan number of parent scan in case of recursively
      stored scans (\code{msLevel>1})
      \item \code{msLevel} - 1- means MS scan, 2- means MS/MS scan,
      etc.
      \item \code{header} - xml code of <scan> header might contain
      other useful attributes
      \item \code{maldi} - optional - acquisition dependent properties of a 
      MALDI experiment
      \item \code{scanOrigin} - optional - name of parent file(?)
      \item \code{precursorMz} - optional - information about the precursor ion
      \item \code{nameValue} - optional - properties of the scan not included 
      elsewhere 
    }
    All optional elements contain unparsed XML code, if corresponding 
    sections are present, or \code{NULL}. See mzXML schema and documentation for 
    more details
   }
  \item{header}{Stores header of <mzXML> section containing information about
    namespace and schema file location.}
  \item{msInstrument}{General information about the MS instrument. Stored as XML.}
  \item{parentFile}{Path to all the ancestor files. Stored as XML.}
  \item{dataProcessing}{Description of any data manipulation. Stored as XML.}
  \item{separation}{Information about the separation technique. Stored as XML.}
  \item{spotting}{Acquisition independent properties of a MALDI experiment. Stored as XML.}
  \item{indexOffset}{Offset of the index element. Either 0 or a vector.}
}

\references{ 
  Definition of \code{mzXML} format:
  \url{http://tools.proteomecenter.org/mzXMLschema.php}
  
  Documentation of \code{mzXML} format:
  \url{http://sashimi.sourceforge.net/schema_revision/mzXML_2.1/Doc/mzXML_2.1_tutorial.pdf}
   
  More Documentation of \code{mzXML} format:
  \url{http://sashimi.sourceforge.net/software_glossolalia.html}
  
  ReadmzXML software \url{http://tools.proteomecenter.org/readmzXML.php}
}
\author{Jarek Tuszynski (SAIC) \email{jaroslaw.w.tuszynski@saic.com}} 

\seealso{
  \code{\link[caMassClass]{read.mzXML}} from \pkg{caMassClass} package for original
  documentation and examples.
   
  For reading XML files see \code{\link[XML]{xmlTreeParse}} from \pkg{XML}.
  
  Other R function related to mzXML format: \code{\link[xcms]{xcmsRaw}} from 
  \pkg{xcms} BioConductor package.
}

\examples{
## get examples directory 
exampleDirectory <- system.file("Examples", package="readMzXmlData");

## read example spectrum
xml <- readMzXmlData:::.read.mzXML(file.path(exampleDirectory,
    "A1-0_A1.mzXML"));
xml
  
## plot spectrum of scan 1
plot(xml$scan[[1]]$mass, xml$scan[[1]]$peaks, type="l");
  
## original example code of caMassClass package:
## extracting data from unparsed sections
tree <- xmlTreeParse(xml$msInstrument, asText=TRUE, asTree=TRUE);
x <- xmlRoot(tree);
xmlName(x);
xmlAttrs(x[["msManufacturer"]]) ["value"];
xmlAttrs(x[["software"]]);
}
\keyword{file}

