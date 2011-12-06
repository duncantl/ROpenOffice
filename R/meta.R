setGeneric("creationTime",
            function(doc, ...)
               standardGeneric("creationTime"))


setGeneric("creator",
            function(doc, ...)
               standardGeneric("creator"))


setMethod("creationTime", "character",
           function(doc, ...) {
             creationTime(ooDoc(doc))
           })

setMethod("creator", "character",
           function(doc, ...) {
             creator(ooDoc(doc))
           })


setMethod("creationTime", "OODocument",
            function(doc, ...) {
               xml = xmlParse(doc[["meta.xml"]], asText = TRUE)
               nodes = getNodeSet(xml, "//meta:creation-date", c(meta = "urn:oasis:names:tc:opendocument:xmlns:meta:1.0"))
               strptime(xmlValue(nodes[[1]]), "%Y-%m-%dT%H:%M:%S")
            })

setMethod("creator", "OODocument",
            function(doc, ...) {
               xml = xmlParse(doc[["meta.xml"]], asText = TRUE)
               nodes = getNodeSet(xml, "//meta:initial-creator", c(meta = "urn:oasis:names:tc:opendocument:xmlns:meta:1.0"))
               if(length(nodes))
                  xmlValue(nodes[[1]])
               else
                 NA_character_
            })
              

setGeneric("creator<-", 
           function(doc, ..., value)
               standardGeneric("creator<-"))


setMethod("creator<-", "character",
           function(doc, ..., value) {
             creator(ooDoc(doc)) = value
             doc
           })

setMethod("creator<-", "OODocument",
           function(doc, ..., value) {
             xml = xmlParse(doc[["meta.xml"]], asText = TRUE)
             nodes = getNodeSet(xml, "//meta:initial-creator", c(meta = "urn:oasis:names:tc:opendocument:xmlns:meta:1.0"))
             xmlValue(nodes[[1]]) = value
             doc[["meta.xml"]] = saveXML(xml)
             doc
           })
