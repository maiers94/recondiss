library(openxlsx)
library(e1071)
data <- read.xlsx("ds_new.xlsx",2)
milan <- data[[1]]
sandp.tsx <- data[[2]]
dax <- data[[3]]
dow <- data[[4]]
nikkei <-data[[5]]
#create a window of 2 years take 2013-2014

sample <- dax[12523:13827]
#2008 - 2012
plot(sample,type="l")

sample.pre <- function(input){
  output <- input[1]
  cut <- 0
  for(i in 2:length(input)){
    if(input[i-1]!=input[i]){
      output <- c(output,input[i])
    }
    else cut <- cut +  1
  }
  print(c("cut input by",cut))
  return(output)
}

sample <- sample.pre(sample)



#simple moving avg
ma <- function(inp,l){
  output <- vector(length = (length(inp)-l))
  for(i in 1:(length(inp)-l)){
    output[i] <- sum(inp[i:(i+len)])/l
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
macd.siggen <- function(input,n,m,rule1=TRUE,rule2=TRUE){
  cd <- macd(input,n,m)
  e9 <- exp(cd,9)
  #cut macd to the right length
  cd <- cd[10:length(cd)]
  print(length(e9))
  print(length(cd))
  #find the current position of the MACD
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
macd.trader <- function(sample,signal){
  #cut the data to fit the signal
  #data <- rev(rev(sample)[1:length(signal)])
  data<-sample
  i <- 1
  buy.returns <- vector()
  sell.returns <- vector()
  while(i < length(signal)-10){
    #buy signal
    if(signal[i]==1){
      buy.ret <- log(data[i+10])-log(data[i])
      buy.returns <- c(buy.returns,buy.ret)
      i<-i+10
    }
    #sell signal
    if(signal[i]==-1){
      sell.ret <- log(data[i+10])-log(data[i])
      sell.returns <- c(sell.returns,sell.ret)
      i<- i + 10
    }
    i <- i+1
  }
  returns <- list(buy.returns,sell.returns)
  return(returns)
}

trader.alt <- function(sample,signal){
  position <- 0
  data<-sample
  i <- 1
  buy.returns <- vector()
  sell.returns <- vector()
  short.open <- FALSE
  long.open <- FALSE
  hold.time <- vector()
  
  while(i < length(signal)){
    #buy signal
    if(signal[i]==1){
      open.l <- i
      long.open <- TRUE
      position <- "long"
    }
    #sell signal
    if(signal[i]==-1){
      open.s <- i
      short.open <- TRUE
      position <- "short"
    }
    if(short.open ==TRUE && position == "long"){
      sell.ret <- log(data[i])-log(data[open.s])
      sell.returns <- c(sell.returns,sell.ret)
      short.open <- FALSE
      hold.time <- c(hold.time,i-open.s)
    }
    if(long.open ==TRUE && position == "short"){
      buy.ret <- log(data[i])-log(data[open.l])
      buy.returns <- c(buy.returns,buy.ret)
      long.open <- FALSE
      hold.time <- c(hold.time,i-open.l)
    }
    
    i <- i+1
  }
  if(long.open == TRUE){
    buy.ret <- log(data[length(signal)])-log(data[open.l])
    buy.returns <- c(buy.returns,buy.ret)
    long.open <- FALSE
  }
  if(short.open == TRUE){
    sell.ret <- log(data[length(signal)])-log(data[open.s])
    sell.returns <- c(sell.returns,sell.ret)
    long.open <- FALSE
  }
  print(mean(hold.time))
  returns <- list(buy.returns,sell.returns,mean(hold.time))
  return(returns)
  
  return(returns)
}

#buy and hold avg returns
buyandhold <- function(sample,n){
  returns <- vector()
  i <- 1
  while(i+n <= length(sample)){
    ret <- log(sample[i+n])-log(sample[i])
    returns <- c(returns,ret)
    i <- i + n + 1 
  }
  # print(kurtosis(returns,1))
  # print(skewness(returns,1))
  # print(sd(returns))
  # print(length(returns))
  hist(returns)
  return(mean(returns))
}

#summary, example of the trading rule macd(12,26,0)
summary <-function() {
  sig <- macd.siggen(sample,26,12,rule2=FALSE)
  bh <- buyandhold(sample,10)
  macd1 <- macd.trader(sample,sig)
  plot(macd(sample,26,12),type="l")
  macd2 <- trader.alt(sample,sig)
  a <- buyandhold(sample,macd2[[3]])
  
  print("B&H Avg Returns, Buy Strategy returns, Sell Startegy Returns, Strategy Returns")
  print(c(bh,mean(macd1[[1]]),mean(macd1[[2]]),mean(c(macd1[[1]],-1*macd1[[2]]))))
  print("B&H Avg Returns, Buy Strategy + returns, Sell Startegy + Returns, Strategy + Returns")
  print(c(a,mean(macd2[[1]]),mean(macd2[[2]]),mean(c(macd2[[1]],-1*macd2[[2]]))))
}

#####################
