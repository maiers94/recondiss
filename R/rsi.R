#'A function to compute the relative strength index
#'
#'@param input Input time series
#'@param n length of RSI
#'@export
rsi <- function(input,n){
  output <- vector(length=(length(input)-n))
  for(t in n:length(input)){
    differences <- vector(length=(n-1))
    nulls <- vector(length=(n-1))
    for(j in 1:(n-2)){
      differences[j] <- input[t-j]-input[t-j-1]
      if(input[t-j]-input[t-j-1]>0)nulls[j] <- 1
      if(input[t-j]-input[t-j-1]<=0)nulls[j] <- 0
    }
    num <- differences*nulls
    den <- abs(differences)
    output[t-n+1] <- 100*sum(num)/sum(den)
  }
  return(output)
}

#'@export
rsi.siggen <- function(input,n,rule1=FALSE,rule2=FALSE){
  rs <- rsi(input,n)

  #find the current position of the rsi
  if(rs[1]>50)pos <- TRUE
  if(rs[1]<50)pos <- FALSE
  if(rs[1]<30)tpos <- -1
  else if(rs[1]>70)tpos <- 1
  else tpos <- 0

  signals <- vector(length=length(input))
  #fill first n elements with 0, no signals are created before the index exists
  signals[1:(n-1)] <- rep(0,(n-1))
  i<-2
  #now run actual analysis
  while(i < length(rs)){
    #RULE1 Buy signal if rsi crosses centreline

    index <- i+n-1
    if(rule1 == TRUE){
      if(pos == TRUE){
        if(rs[i]<50){
          signals[index] <- -1
        }
      }
      if(pos == FALSE)
        if(rs[i]>50){
          signals[index] <- 1
        }
    }

    if(rule2 == TRUE){
      if(tpos==1){
        if(rs[i]<70){
          signals[index] <- -1
        }
      }
      if(tpos==-1){
        if(rs[i]>30){
          signals[index] <- 1
        }
      }
    }

    if(signals[index]!=1 && signals[index]!=-1){
      signals[index]<-0
    }
    if(rs[i]>50)pos <- TRUE
    if(rs[i]<50)pos <- FALSE
    if(rs[i]<30)tpos <- -1
    else if(rs[i]>70)tpos <- 1
    else tpos <- 0
    i <- i+1
  }
  return(signals)
}
