rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
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
    
    ## Return hospital name in that state with the given rank 30-day death rate
    result <- c(NA)
    
    df <- data.frame(hname=data[,2], state=data[,7], oc=data[,coln])
    df <- df[df$state == state,]
    df <- df[df$oc != "Not Available",]
    
    score <- df[order(as.numeric(as.vector(df$oc)), df$hname), ]
    nr <- nrow(score)
    if (num == "best") {
        num <- 1
    } else if (num == "worst") {
        num <- nr
    }
    if (num <= nr ) {
        result <- as.vector(score[num,][1]$hname)
    }
    
    return (result)
}
