library(ROpenOffice)

f = system.file("sampleData", "missingWithRowNames.ods", package = "ROpenOffice")
d = read.ods(f)
stopifnot(all(dim(d) == c(9L, 3L)))

stopifnot(length(rownames(d)) > 0)
stopifnot(all(rownames(d) == c("A", "B", "C", "D", "E", "W", "X", "Y", "Z")))


f = system.file("sampleData", "mtcars.ods", package = "ROpenOffice")
d = read.ods(f)
   # the original mtcars.csv file was written with 2 digits so read into OpenOffice
   # without the same values as in R.
all.equal(d, mtcars, tolerance = .1)


f = system.file("sampleData", "missingColumns.ods", package = "ROpenOffice")
d = read.ods(f)
length(d)
names(d)
sapply(d, dim)
# 2 sheets.


