complete <- function(directory, id = 1:332) {
    vect <- c("0","0")
    nbs <- c()
    for (i in id) {
        ncount <- 0
        fname <- as.character(i)
        nc <- nchar(fname)
        if ( nc < 3) {
            fname <- paste(paste(vect[1:(3-nc)],collapse=""),fname,sep="")
        }
        fvect = c(directory,"/",fname, ".csv")
        fpath = paste(fvect,collapse="")
        ##print(fpath)
        data <- read.csv(file=fpath,head=TRUE,sep=",")
        nr <- nrow(data)
        for (i in 1:nr) {
            nasum <- sum(as.numeric(is.na(data[i,])))
            if (nasum == 0) {
                ncount <- ncount + 1
            }
        }
        nbs <- union(nbs, ncount)
    }
    df <- data.frame(id, nbs)
    return(df)
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    vect <- c("0","0")
    tsum <- 0
    count <- 0
    pmean <- 0
    result <- c()
    sumvect <- c()
    countvect <- c()
    if (!is.null(id)) {
        mx <- max(id)
        mxlen <- nchar(as.character(mx))
        for (i in id) {
            fname <- as.character(i)
            nc <- nchar(fname)
            ##print(nc)
            if ( nc < 3) {
                fname <- paste(paste(vect[1:(3-nc)],collapse=""),fname,sep="")
            }
            fvect = c(directory,"/",fname, ".csv")
            fpath = paste(fvect,collapse="")
            ##print(fpath)
            data <- read.csv(file=fpath,head=TRUE,sep=",")
            fdata <- data[pollutant]
            tdata <- fdata[!is.na(fdata)]
            ##if (sum(tdata))
            ##tsum <- sum(tsum, sum(tdata))
            ##count <- sum(count, length(tdata))
            tsum <- sum(tdata)
            tcount <- length(tdata)
            sumvect <- append(sumvect, tsum)
            countvect <- append(countvect, tcount)
            if (tcount < 0) {
                r <- tsum/tcount;
                result <- append(result, r)
            }
        }
        pmean <- sum(sumvect)/sum(countvect)
    }
    result <- round(pmean,digits=3)
    return(result)
}

