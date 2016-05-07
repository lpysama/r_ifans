library(RMySQL)

xx1=read.xlsx("F:\\lpy2.xlsx",sheetIndex = 1,encoding = "UTF-8")
xx1$date=as.Date(as.POSIXct(xx1$starttime, origin="1970-01-01"))
xx1$start=as.POSIXct(xx1$starttime, origin="1970-01-01")
xx1$end