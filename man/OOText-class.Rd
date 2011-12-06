\name{OOText-class}
\Rdversion{1.1}
\docType{class}
\alias{OODocument-class}
\alias{OOText-class}
\alias{OOPresentation-class}
\alias{OOSpreadsheet-class}
\alias{getTables,OOText-method}
\alias{getTitle}
\alias{getTitle,OOText-method}
\alias{getTitle,OOText-method}
\alias{getTitle,character-method}
\alias{getTitle,XMLInternalDocument-method}

\alias{getTables}
\alias{getTables,OOText-method}
\alias{getTables,character-method}
\alias{getTables,XMLInternalDocument-method}

\title{Classes for different types of Open Office documents}
\description{
  These classes represent the different types of documents
  that are handled by the Office Open format.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OOText", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"character"}  }
    \item{\code{readTime}:}{Object of class \code{"POSIXct"} }
    \item{\code{classes}:}{Object of class \code{"character"} }
    \item{\code{elements}:}{Object of class \code{"character"} }
    \item{\code{info}:}{Object of class \code{"data.frame"} }
  }
}
\section{Extends}{
Class \code{"\linkS4class{OODocument}"}, directly.
Class \code{"\linkS4class{ZipFileArchive}"}, by class "OODocument", distance 2.
Class \code{"\linkS4class{character}"}, by class "OODocument", distance 3.
Class \code{"\linkS4class{ZipArchive}"}, by class "OODocument", distance 3.
Class \code{"\linkS4class{vector}"}, by class "OODocument", distance 4.
Class \code{"\linkS4class{data.frameRowLabels}"}, by class "OODocument", distance 4.
Class \code{"\linkS4class{SuperClassMethod}"}, by class "OODocument", distance 4.
}
\section{Methods}{
  \describe{
    \item{getTables}{\code{signature(doc = "OOText")}: methods for
      reading tables from a word-processing document }
    \item{getTitle}{\code{signature(doc = "OOText")}: methods to get the
    title of an office open document}
	 }
}
\references{
}
\author{
Duncan Temple Lang
}


\seealso{
  \code{\link{read.ods}}
}
\examples{
showClass("OOText")
}
\keyword{classes}
