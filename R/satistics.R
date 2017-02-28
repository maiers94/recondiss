
teststat <- function(x,y){
  mux <- mean(x)
  muy <- mean(y)
  totvar <- var(c(x,y))
  nx <- length(x)
  ny <- length(y)
  t <- (mux - muy) / sqrt(totvar / nx + totvar / ny)
  return(t)
}

#'@importFrom e1071 kurtosis
#'@importFrom e1071 skewness
#'@export
analyse.subperiod <- function(per,ind,dat,rsi1=FALSE,macd=FALSE,rsi2=FALSE){
  ratio <- function(returns){
    pos <- length(returns[returns>0])
    ratio <- pos/length(returns)
    return(ratio)
  }

  output <- function(var){
    print(c("N(buy),N(sell): ",length(var[[1]]),length(var[[2]])))
    print(c("Mean buy/sell: ", mean(var[[1]]),mean(var[[2]])))
    bt <- teststat(var[[1]],bah)
    st <- teststat(var[[2]],bah)
    df1 <- length(var[[1]]) + length(bah) - 2
    df2 <- length(var[[2]]) + length(bah) - 2
    print(c("t-values for means: (b / s) ",bt,st))
    print(c("p-values:",pt(-abs(bt),df1),pt(-abs(st),df2)))
    br <- ratio(var[[1]])
    sr <- ratio((-1*var[[2]]))
    print(c("buy>0 / sell>0: ",br," / ",sr))
    bminusst <- teststat(var[[1]],var[[2]])
    bminuss <- mean(var[[1]]) - mean(var[[2]])
    dftot <- length(var[[2]]) + length(var[[2]]) - 1
    print(c("Buy-Sell Strategy: (t-value)",bminuss,bminusst))
    print(c("p-value:",pt(-abs(bt),dftot)))
    return(0)
  }



  cur <- dat[[ind]][[per]]
  bah <- buyandhold(cur,10)
  print("Unconditional Returns for the Period: (mean,sd,skew,kurt) ")
  print(mean(bah))
  print(sd(bah))
  print(skewness(bah))
  print(kurtosis(bah))

  hist(bah, main="10-day Unconditional Returns",xlab="returns",breaks=10)
  print("##########")

  if(macd==TRUE){
    macd1sig <- macd.siggen(cur,26,12,rule2=FALSE)
    macd2sig <- macd.siggen(cur,26,12,rule1=FALSE)
    macd1 <- macd.trader(cur, macd1sig)
    macd2 <- macd.trader(cur, macd2sig)
    print("MACD(26,12,0) data:")

    output(macd1)

    print("##########")
    print("MACD(26,12,9) data:")
    output(macd2)
  }


  if(rsi1==TRUE){
    rsi1sig <- rsi.siggen(cur,7,rule1=TRUE)
    rsi2sig <- rsi.siggen(cur,14,rule1=TRUE)
    rsi3sig <- rsi.siggen(cur,21,rule1=TRUE)
    rsi1 <- trader(cur, rsi1sig)
    rsi2 <- trader(cur, rsi2sig)
    rsi3 <- trader(cur, rsi3sig)
    print("##########")
    print("RSI(7,50) data:")
    output(rsi1)
    print("##########")
    print("RSI(14,50) data:")
    output(rsi2)
    print("##########")
    print("RSI(21,50) data:")
    output(rsi3)
  }
  if(rsi2==TRUE){
    rsi4sig <- rsi.siggen(cur,7,rule2=TRUE)
    rsi5sig <- rsi.siggen(cur,14,rule2=TRUE)
    rsi6sig <- rsi.siggen(cur,21,rule2=TRUE)
    rsi4 <- trader(cur, rsi4sig)
    rsi5 <- trader(cur, rsi5sig)
    rsi6 <- trader(cur, rsi6sig)
    print("##########")
    print("RSI(7,30/70) data:")
    output(rsi4)
    print("##########")
    print("RSI(14,30/70) data:")
    output(rsi5)
    print("##########")
    print("RSI(21,30/70) data:")
    output(rsi6)
  }
  return(0)
}

#'@export
wholeperiod <- function(a){
  b <- a[[1]]
  for(i in 2:5){
    b <- c(b,a[[i]])
  }
  b <- list(list(b))
  return(b)
}

trading.costs <- function(returns,p){}
