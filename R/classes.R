
setClass("OODocument", contains = "ZipFileArchive")
setClass("OOSpreadsheet", contains = "OODocument")
setClass("OOText", contains = "OODocument")
setClass("OOPresentation", contains = "OODocument")

ooDoc =
function(file)
{
  z = zipArchive(file)
  class = switch(z[["mimetype"]],
                 "application/vnd.oasis.opendocument.spreadsheet" = "OOSpreadsheet",
                 "application/vnd.oasis.opendocument.text" = "OOText",
                 "application/vnd.oasis.opendocument.presentation" = "OOPresentation",
                 stop("Not yet"))
  new(class, z)
}

