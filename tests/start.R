if(FALSE) {
wb = zipArchive("~/Spreadsheet.ods", class = "OOWorkbook")

getNodeSet(tb[[1]], "./table:table-row[./table:table-cell[@office:value]]",
                 OpenOfficeNamespaces[c("office", "table")])
}
