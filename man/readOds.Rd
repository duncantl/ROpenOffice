\name{read.ods}
\alias{read.ods}
\alias{convertTable}
\title{Read the sheets in an Open Office workbook}
\description{
  This function is a \dQuote{quick} implementation of
 reading all of the spreadsheets within an
 Open Office workbook. It reads each sheet into a
 data frame and returns these as a list.
}
\usage{
read.ods(file, header = TRUE, simplify = TRUE,
          doc = xmlParse(zipArchive(file)[["content.xml"]]),
           stringsAsFactors = TRUE)
}
\arguments{
  \item{file}{the name of the \code{.ods} file} 
  \item{header}{a logical value indicating whether the first row is a }
  \item{simplify}{a logical value which is intended to allow the caller
    retrieve a data.frame object rather than a list containing one data.frame
    when there is only one worksheet in the workbook.}
  \item{doc}{the parsed XML document containing the content of the work book.
      This is a parameter to allow caller who are processing other aspects of the
      workbook to pass the already parsed XML document to this function,
      perhaps having modified it.}
  \item{stringsAsFactors}{a logical value controlling whether variables
    in the result data frame are converted to a factors if they are
    strings.
    This is passed to \code{\link[base]{as.data.frame}}.}
}
\details{
 This uses the Rcompression package to access the files within the archive
 and then 
}
\value{
 If \code{simplify} is \code{TRUE} and 
  the workbook contains a single work sheet, a data frame is returned;
  otherwise, a list in which each element is a data frame is returned.
}
\references{ ~put references to the literature/web site here ~ }
\author{Duncan Temple Lang}

\seealso{
 \code{xmlParse}, \code{getNodeSet}
}
\examples{
  f = system.file("sampleData", "Spreadsheet.ods", package = "ROpenOffice")
  read.ods(f, FALSE)
}
\keyword{IO}
\keyword{data}