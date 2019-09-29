### Dataqua-2002 adatimport
### ~/Kutatás/binHidegviz/ -ben fejlesztve.
waqImport <- function(filename, freq.in.sec = 60, trunc.unit="min", Correction = 10){
    require(xts)
    ## Determination of size in bytes
    size <- file.info(filename)$size
    stopifnot(size > 0) ## check case of empty file
    ### Process header
    newdata <- file(filename, "rb")
    rawHead <- readBin(newdata, "raw", n=158)
    rawDate <- as.character(readBin(newdata, "raw", n=5))
    ## Process date and time
    StartDate <- paste(paste0("20", rawDate[1]),
                       rawDate[2],
                       rawDate[3],
                       sep="-")
    StartTime <- paste(rawDate[4],
                       rawDate[5],
                       sep=":")
    AscStartDateTime <- paste(StartDate, StartTime)
    StartDateTime <- as.POSIXct(AscStartDateTime)
    LoggerStart <- trunc(StartDateTime, trunc.unit)
    ## If frequenci different from trunc.unit, treat differences works 30 min
    TimeDiff <- difftime(StartDateTime,LoggerStart, units="sec") 
    freq.multi <- ifelse( TimeDiff > freq.in.sec, 2, 1)
    LoggerStart <- LoggerStart + freq.multi * freq.in.sec
    ## Continue processing with dropping 01 and 00
    readBin(newdata, "raw", n=2)
    ## Read number of stored records
    rawDataLength <- readBin(newdata, "integer", size=2)
    ## 5 db 0, 90, 0, 90, 52, b1, number What is the function???
    ## Drop unknown thing
    readBin(newdata, "raw", n=11)
    ## Drop separator 0
    readBin(newdata, "raw", n=1)
    ## Read end date
    rawEndDate <- as.character(readBin(newdata, "raw", n=5))
    ## Process end date
    AscEndDateTime <-  paste(paste(paste0("20",rawEndDate[1]),
                rawEndDate[2],
                rawEndDate[3],sep="-"),
          paste(rawEndDate[4], rawEndDate[5], sep=":")
          )
    ## Echo filename, start end end date
    cat(paste(filename, AscStartDateTime, AscEndDateTime, "\n"))
    ## After end date some unknown code: bc, 74, 13, 3c , 0, 0, 40, 3f ...
    readBin(newdata, "raw", n=76)
    rawData <- readBin(newdata, integer(), size=2, n=rawDataLength)
    close(newdata)
    ## Generate Date Series
    DateSeries <- seq(LoggerStart,
                      by = paste(freq.in.sec, "sec"),
                      length.out = rawDataLength
                      )
    ## Final xts, data modified with some constant divider.
    xts(rawData/Correction, DateSeries)
}
