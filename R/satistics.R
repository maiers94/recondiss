
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
analyse.subperiod <- function(per,ind,dat,rsi=FALSE){
  ratio <- function(returns){
    pos <- length(returns[returns>0])
    ratio <- pos/length(returns)
    return(ratio)
  }

  cur <- dat[[ind]][[per]]
  bah <- buyandhold(cur,10)
  print("Unconditional Returns for the Period: (mean,sd,skew,kurt) ")
  print(mean(bah))
  print(sd(bah))
  print(skewness(bah))
  print(kurtosis(bah))
  print("##########")
  macd1sig <- macd.siggen(cur,26,12,rule2=FALSE)
  macd2sig <- macd.siggen(cur,26,12,rule1=FALSE)
  macd1 <- macd.trader(cur, macd1sig)
  macd2 <- macd.trader(cur, macd2sig)
  print("MACD(26,12,0) data:")
  print(c("N(buy),N(sell): ",length(macd1[[1]]),length(macd1[[2]])))
  print(c("Mean buy/sell: ", mean(macd1[[1]]),mean(macd1[[2]])))
  bt <- teststat(macd1[[1]],bah)
  st <- teststat(macd1[[2]],bah)
  df1 <- length(macd1[[1]]) + length(bah) - 2
  df2 <- length(macd1[[2]]) + length(bah) - 2
  print(c("t-values for means: (b / s) ",bt,st))
  print(c("p-values:",pt(-abs(bt),df1),pt(-abs(st),df2)))

  #print(t.test(macd1[[1]],bah,var.equal=TRUE))

  br <- ratio(macd1[[1]])
  sr <- ratio((-1*macd1[[2]]))
  print(c("buy>0 / sell>0: ",br," / ",sr))
  bminusst <- teststat(macd1[[1]],macd1[[2]])
  bminuss <- mean(macd1[[1]]) - mean(macd1[[2]])
  dftot <- length(macd1[[2]]) + length(macd1[[2]]) - 1
  print(c("Buy-Sell Strategy: (t-value)",bminuss,bminusst))
  print(c("p-value:",pt(-abs(bt),dftot)))

  print("##########")
  print("MACD(26,12,9) data:")
  print(c("N(buy),N(sell): ",length(macd2[[1]]),length(macd2[[2]])))
  print(c("Mean buy/sell: ", mean(macd2[[1]]),mean(macd2[[2]])))
  bt <- teststat(macd2[[1]],bah)
  st <- teststat(macd2[[2]],bah)
  df1 <- length(macd2[[1]]) + length(bah) - 2
  df2 <- length(macd2[[2]]) + length(bah) - 2
  print(c("t-values for means: (b / s) ",bt,st))
  print(c("p-values:",pt(-abs(bt),df1),pt(-abs(st),df2)))

  br <- ratio(macd2[[1]])
  sr <- ratio((-1*macd2[[2]]))
  print(c("buy>0 / sell>0: ",br," / ",sr))
  bminusst <- teststat(macd2[[1]],macd2[[2]])
  bminuss <- mean(macd2[[1]]) - mean(macd2[[2]])
  dftot <- length(macd2[[2]]) + length(macd2[[2]]) - 1
  print(c("Buy-Sell Strategy: (t-value)",bminuss,bminusst))
  print(c("p-value:",pt(-abs(bt),dftot)))



  if(rsi==TRUE)print(rsi) #to be edited


  return(0)
}

trading.costs <- function(returns,p){}
