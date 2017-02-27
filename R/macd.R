#simple moving avg
ma <- function(inp,l){
  output <- vector(length = (length(inp)-l))
  for(i in 1:(length(inp)-l)){
    output[i] <- sum(inp[i:(i+l)])/l
  }
  return(output)
}
#exponential moving average
exp <- function(inp,l){
  output <- vector(length = (length(inp)-l))
  output[1] <- sum(inp[1:l])/l
  alpha <- 2/l
  for(i in 2:(length(inp)-l)){
    cur <- output[i-1]
    output[i] <- (inp[i+l]-output[i-1])*alpha + output[i-1]
  }

  return(output)
}

#Moving Average Convergence-divergence
macd <- function(data,n,m){
  #n>m, in line with Chong,Ng (2014)
  eman <- rev(exp(data,n))
  emam <- rev(exp(data,m))
  macd <- vector(length=length(eman))
  for(i in 1:length(eman)){
    macd[i] <- emam[i]-eman[i]
  }
  macd <- rev(macd)
  return(macd)
}

#this generates the trading signals
#use MACD(12,26,0) and MACD(12,26,9)
#'@export
macd.siggen <- function(input,n,m,rule1=TRUE,rule2=TRUE){
  cd <- macd(input,n,m)
  e9 <- exp(cd,9)
  #cut macd to the right length
  cd <- cd[10:length(cd)]
  #print(length(e9))
  #print(length(cd))
  #find the current position of the MACD
  #print(cd[1])
  if(cd[1]>0)pos <- TRUE
  if(cd[1]<0)pos <- FALSE
  if(cd[1]>e9[1])hi <- TRUE
  if(cd[1]<e9[1])hi <- FALSE

  signals <- vector(length=length(input))
  #fill first n elements with 0, no signals are created before the index exists
  signals[1:(n+10)] <- rep(0,(n+10))
  i<-2
  #now run actual analysis
  while(i < length(cd)){
    #RULE1 Buy signal if macd crosses zero from below/sell if from above
    #RULE2 Buy signal if macd crosses em(9) from below/sell from above
    index <- i+9+n
    if(rule1 == TRUE){
      if(pos == TRUE){
        if(cd[i]<0){
          signals[index] <- -1
        }
      }
      if(pos == FALSE)
        if(cd[i]>0){
          signals[index] <- 1
        }
    }
    if(rule2 == TRUE){
      if(hi == TRUE){
        if(cd[i]<e9[i]){
          signals[index] <- -1
        }
      }
      if(hi == FALSE)
        if(cd[i]>e9[i]){
          signals[index] <- 1
        }
    }

    if(signals[index]!=1 && signals[index]!=-1){
      signals[index]<-0
    }
    if(cd[i]>0)pos <- TRUE
    if(cd[i]<0)pos <- FALSE
    if(cd[i]>e9[1])hi <- TRUE
    if(cd[i]<e9[1])hi <- FALSE
    i <- i+1
  }
  return(signals)
}

#trading average returns
#'@export

