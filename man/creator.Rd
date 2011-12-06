\name{creator}
\alias{creator}
\alias{creationTime}
\alias{creator<-}
\alias{creationTime,OODocument-method}
\alias{creationTime,character-method}
\alias{creator,OODocument-method}
\alias{creator,character-method}
\alias{creator<-,OODocument-method}
\alias{creator<-,character-method}


\title{Access information about an Office Open document}
\description{
  These functions and methods provide access to simple meta information
  about an Office Open document such as the creator and when the
  document was created.
  We can also set the name of the creator.

  These serve as examples of how to access the meta data.
}
\usage{
creator(doc, ...)
}
\arguments{
  \item{doc}{the Office Open document specified as a
  file name or an instance of one of the document classes, e.g. OOText,
  OOSpreadSheet or OOPresentation}
  \item{\dots}{additional arguments for methods.}
}
\value{
 \code{creator} returns a string giving the name of the initial author
 of the document.

 \code{creationTime} returns an object of class POSIXt, specifically POSIXlt.
}
\references{
}
\author{
Duncan Temple Lang
}


\seealso{}
\examples{
}
\keyword{interface}
