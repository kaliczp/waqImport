### Dataqua-2002 adatimport
### ~/Kutatás/binHidegviz/ -ben fejlesztve.
waqImport <- function(filename, freq.in.sec = 60, trunc.unit="min", Correction = 10){
    require(xts)
    ### Fejléc feldolgozása
    newdata <- file(filename, "rb")
    rawHead <- readBin(newdata, "raw", n=158)
    rawDate <- as.character(readBin(newdata, "raw", n=5))
    ## Még utána van egy 01 és egy 00 szünet
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
    TimeDiff <- difftime(StartDateTime,LoggerStart, units="sec") 
    freq.multi <- ifelse( TimeDiff > freq.in.sec, 2, 1)
    LoggerStart <- LoggerStart + freq.multi * freq.in.sec
    readBin(newdata, "raw", n=2) ## 01 és 00 eldobása
    rawDataLength <- readBin(newdata, "integer", size=2) ## Az eltárolt adatok száma!
    ## 5 db 0, 90, 0, 90, 52, b1, szám MI LEHET???
    ## Egyelőre eldobom
    readBin(newdata, "raw", n=11) ## fentiek eldobása
    ## elválasztó 0
    readBin(newdata, "raw", n=1) ## fentiek eldobása
    rawEndDate <- as.character(readBin(newdata, "raw", n=5))
    AscEndDateTime <-  paste(paste(paste0("20",rawEndDate[1]),
                rawEndDate[2],
                rawEndDate[3],sep="-"),
          paste(rawEndDate[4], rawEndDate[5], sep=":")
          )
    cat(paste(filename, AscStartDateTime, AscEndDateTime, "\n"))
    ## Utána: bc, 74, 13, 3c , 0, 0, 40, 3f ...
    readBin(newdata, "raw", n=76)
    rawData <- readBin(newdata, integer(), size=2, n=rawDataLength)
    close(newdata)
    ## Idők előállítása
    DateSeries <- seq(LoggerStart,
                      by = paste(freq.in.sec, "sec"),
                      length.out = rawDataLength
                      )
    ## Az idősor előállítása közben osztás a korrekcióval
    xts(rawData/Correction, DateSeries)
}
