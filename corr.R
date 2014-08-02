corr <- function(directory, threshold = 0) {
    vect <- c("0","0")
    nvect <- c()
    svect <- c()
    corrvect <- c()
    id <- 1:332
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
                nvect <- append(nvect,data[j,2])
                svect <- append(svect,data[j,3])
            }
        }
        if (ncount > threshold) {
            corrvect <- append(corrvect, cor(nvect,svect))
        }
        nvect <- c()
        svect <- c()
    }
    return(corrvect)
}