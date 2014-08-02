complete <- function(directory, id = 1:332) {
    vect <- c("0","0")
    nobs <- c()
    rid <- c()
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
        for (j in 1:nr) {
            nasum <- sum(as.numeric(is.na(data[j,])))
            if (nasum == 0) {
                ncount <- ncount + 1
            }
        }
        ##if (ncount > 0) {
            nobs <- append(nobs, ncount)
            rid <- append(rid, i)
        ##}
    }
    id <- rid
    df <- data.frame(id, nobs)
    return(df)
}