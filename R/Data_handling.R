#'@importFrom openxlsx read.xlsx
#'@importFrom graphics plot
#'@export
preprocessing <- function(){
  data <- read.xlsx(system.file("extdata","ds_new.xlsx",package="recondiss"),2)
  # milan <- data[[1]]
  # sandp.tsx <- data[[2]]
  # dax <- data[[3]]
  # dow <- data[[4]]
  # nikkei <-data[[5]]
  #cut data into windows according to paper
  for(i in 1:5){
    #for each index
    curdat <- data[[i]]
    cur <- list("1"=curdat[1:1566],"2"=curdat[1567:3392],"3"=curdat[3393:4304],"4"=curdat[4305:5479],"5"=curdat[5480:6653])
    nam <- paste("dat", i, sep = "")
     for(j in 1:5){
       #for each of the 5 periods
       cur[[j]] <- sample.pre(cur[[j]])
     }
    assign(nam, cur)

  }
  out <- list(dat1,dat2,dat3,dat4,dat5)

  return(out)
}


sample.pre <- function(input){
  output <- input[1]
  cut <- 0
  for(i in 2:length(input)){
    if(!is.na(input[i])){
      if(input[i-1]!=input[i]){
        output <- c(output,input[i])
      }
      else cut <- cut + 1
    }
    else{
      cut <- cut +  1
      input[i]<- input[i-1]
    }

  }
  print(c("cut input by",cut))
  return(output)
}

#sample <- sample.pre(sample)

trader <- macd.trader <- function(sample,signal){
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



#buy and hold avg returns
#'@importFrom graphics hist
#'@export
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
  return(returns)
}

#summary, example of the trading rule macd(12,26,0)
#'@importFrom graphics plot

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

#old
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

