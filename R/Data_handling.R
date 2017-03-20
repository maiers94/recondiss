#'@importFrom openxlsx read.xlsx
#'@importFrom graphics plot
#'@export
preprocessing <- function(){
  data <- read.xlsx(system.file("extdata","ds_new.xlsx",package="recondiss"),2)
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

trader <- macd.trader <- function(sample,signal){
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
  hist(returns)
  return(returns)
}

