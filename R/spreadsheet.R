# Came from RExcelML
if(FALSE) {
setClass("OOWorkbook", contains = "ZipArchiveEntry")
setClass("OOWorksheet", representation(content = "XMLInternalDocument", name = "ZipArchiveEntry"))
setClass("OOWorksheetFile", contains = "ZipArchiveEntry")

setMethod("names", "OOWorkbook",
            function(x) {
              doc = xmlParse(x[["content.xml"]])
              unlist(getNodeSet(doc, "//table:table/@table:name", "table"))
            })
#setMethod("[[", "Workbook"
}

read.ods =
function(file, header = TRUE, simplify = TRUE,
          doc = xmlParse(zipArchive(file)[["content.xml"]]),
          stringsAsFactors = TRUE)
{
  tb = getNodeSet(doc, "//table:table", "table")
  ans = lapply(tb, read.odsSheet, header = header, stringsAsFactors = stringsAsFactors)

  n = sapply(ans, is.null)
  if(any(n)) {
     ans = ans[!n]
     tb = tb[!n]
  }
  
  if(simplify && length(ans) == 1)
    return(ans[[1]])

  names(ans) = sapply(tb, xmlGetAttr, "name")
  ans
}

read.odsSheet =
  #
  #
  # We need to deal with blank rows and where cells are repeated.
  #   In progress
  #
  # tb is the table:table node.
  #
function(tb, header = TRUE, stringsAsFactors = TRUE) # really a header?
{
    # Get all rows which have a cell with a office:value entry. Otherwise
    # it is an empty row.
  rows = getNodeSet(tb, "./table:table-row[./table:table-cell[@office:value | @office:value-type]]",
                        OpenOfficeNamespaces[c("office", "table")])

  if(length(rows) == 0)
     return(NULL)

  rowNames = FALSE
  varNames = character()
  
  if(header) {
    varNames = xmlSApply(rows[[1]], getCellValue)
#   if(length(varNames) && is.na(varNames[1])) {
#      rowNames = TRUE
#      varNames = varNames[-1]
#   }
    rows = rows[-1]
  } 


     # This gets the types by row and this might be ragged, i.e. not by column
     # Now changed to expand the missing columns so won't be ragged.
  types = t(sapply(rows, function(x) unlist(xmlApply(x, getCellType))))
     # Now get all the cells, expanding the elements that are missing so all the
     # the result will be a matrix
  ans = t(sapply(rows,
                 function(x) {
                    unlist(xmlApply(x, getCellValue), recursive = FALSE)
                  }))

      # find the non-empty columns.
  realCols = apply(ans, 2, function(x) any(!is.na(x)))

  if(!realCols[1]) 
      rowNames = FALSE

  if(header) {
    if((is.na(varNames[1]) || varNames[1] == "") && realCols[1]) 
       rowNames = TRUE
  }  

  
  ans = ans[ , realCols ]
  types = types[, realCols]

  if(!is.matrix(ans)) {
    tp = getColType(types)
    return(if(is.function(tp))
             tp(ans)
           else
             as(ans, tp))
  }  
  
  if(length(varNames))
     varNames = varNames[realCols]


    # convert each of the columns as a set of values.
  tmp = lapply(seq(length = ncol(ans)),
               function(i) {
                  convertColumn(ans[,i], types[, i])
               })

  notNull = !sapply(tmp, is.null)
  tmp = tmp[notNull]
  varNames = varNames[notNull]
  ans = makeDataFrame(tmp, stringsAsFactors = stringsAsFactors)

  
  if(rowNames) {
    rownames(ans) = ans[,1]
    ans = ans[,-1]
    varNames = varNames[-1]
  }

  structure(ans, names = if(length(varNames))
                            as.character(varNames)
                         else
                            paste("V", seq(length = ncol(ans)), sep = ""))
}

getCellType =
function(node)
{

  n = xmlGetAttr(node, "number-columns-repeated", 0)
  txt = xmlGetAttr(node, "value-type", NA)
  if(n > 0)
     rep(txt, n)
  else
     txt
}
  


getCellValue =
  #
  # Get the cell value or a collection of NAs if this is a number-columns-repeated cell.
  #
function(node)
{

  n = xmlGetAttr(node, "number-columns-repeated", 1, as.integer)
  
  if(xmlSize(node) == 0)
    return(rep(NA_character_, n))
  
  date = xmlGetAttr(node, "date-value", NA)
  type = xmlGetAttr(node, "value-type", "")
  val = xmlGetAttr(node, "value", NA_character_)
  
  txt = if(!is.na(date))
          date
        else if(type %in% c("float", "currency", "percentage"))
          val
        else {
           tmp = xmlGetAttr(node, "time-value", xmlGetAttr(node, "value", NA)) # , convertTime)          
           if(is.na(tmp))
              xmlValue(node)
           else
              tmp
        }
 
  if(n > 1)
     rep(txt, n)
  else
     txt
}

convertTime =
function(x)
{
  fmt = sprintf("%s%%HH%%MM%%SS", substring(x, 1, 2))
  as.POSIXct(strptime(x, fmt))
}


Rtypes = c("string" = "character",
           "float" = "numeric",
           "date" = function(x) { as.Date(x, "%Y-%m-%d") },
           "percentage" = "numeric",
           time = convertTime,
           currency = "numeric",
           boolean = "logical"
          )

getColType =
function(types)
{
  types = types[!is.na(types)]
  ans = if(length(types) == 1) {
           Rtypes[[types]]
        } else {
            i = match(types, names(Rtypes))
            Rtypes[[min(i)]]
        }

  if(is.null(ans))
      stop("unrecognized cell type")

  ans
}

convertCellValue =
function(value, type)
{
  if(is.na(value) && is.na(type))
    return(NA)
  
  f = getColType(type)
  if(is.function(f))
     f(value)
  else if(is.character(f))
     as(value, f)
  else
     value
}

convertColumn =
function(col, types)
{
     if(all(is.na(types)))
       return(NULL)
     tp = unique(unlist(na.omit(types)))
     
     if(is.null(tp))
       return(NULL)

     if(length(tp) > 1)
       return(unname(mapply(convertCellValue, col, types, SIMPLIFY = FALSE)))
     
     colType = getColType(tp)
     if(is.function(colType))
       colType(col)
     else if(is.character(colType))
       as(unlist(col), colType)
     else
       unname(mapply(convertCellValue, col, types))
}


makeDataFrame =
   # as.data.frame will take a list column and expand it.  So we need to do this delicately.
   # Possibly work with the vector elements first and then adding columns
function(cols, ...)
{
  i = sapply(cols, is.list)
  if(!any(i))
    return(as.data.frame(cols, ...))

  ans = data.frame(1:length(cols[[1]]))
  for(i in 1:length(cols)) {
    ans[[i]] = cols[[i]]
  }
  ans
}


