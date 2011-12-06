OpenOfficeNamespaces = ROpenOffice:::OpenOfficeNamespaces


setGeneric("getTitle",
function(doc, subtitle = TRUE, ...)
           standardGeneric("getTitle"))

setMethod("getTitle", "character",
            function(doc, subtitle = TRUE, ...) {
              getTitle(ooDoc(doc), subtitle, ...)
            })

setMethod("getTitle", "OOText",
            function(doc, subtitle = TRUE, ...) {
              getTitle(xmlParse(doc[["content.xml"]]), subtitle, ...)
            })

setMethod("getTitle", "XMLInternalDocument",
            function(doc, subtitle = TRUE, ...) {
               q =  c("//text:p[@text:style-name='Title']", "//text:p[@text:style-name='Subtitle']")
               if(!subtitle)
                 q = q[1]
               xpathSApply(doc, paste(q, collapse = " | "),
                                 xmlValue, namespaces = OpenOfficeNamespaces["text"])
})


setGeneric("getTables",
            function(doc, which = NA, ...)
                standardGeneric('getTables'))

setMethod("getTables", "character",
            function(doc, which = NA, ...)
             getTables(ooDoc(doc), which, ...))

setMethod("getTables", "OOText",
            function(doc, which = NA, ...) {
               getTables(xmlParse(doc[["content.xml"]]), which, ...)
            })

setMethod("getTables", "XMLInternalDocument",
            function(doc, which = NA, ...) {              
              nodes = getNodeSet(doc, "//table:table", OpenOfficeNamespaces["table"])

              if(all(is.na(which)))
                return(nodes)

              if(is(which, "numeric") || is(which, "logical"))
                nodes[which]
              else if(is(which, "character")) {
                ids = sapply(nodes, xmlGetAttr, "name")
                i = match(which, ids)
                if(any(is.na(i)))
                  stop("id doesn't map to a table name")
                nodes[i]
              } else
              stop("unhandled type for which")
       })



convertTable =
function(node, header = NA, ...)
{
  rows = getNodeSet(node, "./table:table-row", OpenOfficeNamespaces["table"])
  types = t(sapply(rows, function(x) xmlSApply(x, getCellType)))
  values = t(sapply(rows, function(x) xmlSApply(x, getCellValue)))
    # We have to look at the cells which are empty. We can ignore those when converting to homogeneous
    # columns
  values
}
