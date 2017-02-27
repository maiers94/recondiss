#'A function to compute the relative strength index
#'
#'
rsi <- function(input,n){
  output <- vector(length=(length(input)-n))
  for(i in n:length(input)){
    differences <- vector(length=(n-1))
    nulls <- vector(length=(n-1))
    for(j in 1:(n-2)){
      differences[j] <- input[i-j]-input[i-j-1]
      if(input[i-j]-input[i-j-1]>0)nulls[j] <- 1
      if(input[i-j]-input[i-j-1]<=0)nulls[j] <- 0
    }
    num <- differences*nulls
    den <- abs(differences)
    output[i-n+1] <- 100*sum(num)/sum(den)
  }
    return(output)
}

rsi.siggen <- function(input,n,rule1=TRUE){
  rs <- rsi(input,n)

  #find the current position of the rsi
  if(rs[1]>50)pos <- TRUE
  if(rs[1]<50)pos <- FALSE

  signals <- vector(length=length(input))
  #fill first n elements with 0, no signals are created before the index exists
  signals[1:(n-1)] <- rep(50,(n-1))
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

    if(signals[index]!=1 && signals[index]!=-1){
      signals[index]<-0
    }
    if(rs[i]>50)pos <- TRUE
    if(rs[i]<50)pos <- FALSE
    i <- i+1
  }
  return(signals)
}
