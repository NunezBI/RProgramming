best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome <- tolower(outcome)
    ## Check that state and outcome are valid
    x <- data[,7]   
    y <- x[!is.na(x)]
    y <- as.vector(y)
    if (!(state %in% y)) {
        stop('invalid state')
    }
    v <- seq(11, 24, 6)
    flag <- 0
    coln <- 11
    for (i in v) {
        c <- colnames(data[i])
        c <- gsub("\\."," ",c)
        c <- tolower(c)
        g <- grepl(outcome,c)
        if (g) {
            flag <- 1
            coln <- i
            break;
        }
    }
    if (flag == 0) {
        stop('invalid outcome')    
    }
    ## print(coln)
    ## Return hospital name in that state with lowest 30-day death rate
    df <- data.frame(hname=data[,2], state=data[,7], oc=data[,coln])
    df <- df[df$state == state,]
    df <- df[df$oc != "Not Available",]
    ## ans <- df[which.min(df$oc),][1]
    f <- as.vector(df$oc)
    f <- as.numeric(f)
    f <- which(min(f) == f)
    
    ans <- df[f,][1]
    result <- as.vector(ans$hname)
    
    return (result)
}
