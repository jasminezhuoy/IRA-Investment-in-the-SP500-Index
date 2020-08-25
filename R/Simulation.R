source("R/inflation_adjusted_weighted_return.R")
source("R/Robert_Shiller_adjusted_Return.R")
library(zoo)
library(ggplot2)

adjusted_total$Time <- as.yearmon(paste0(adjusted_total$Month, '/', adjusted_total$Year), format = '%m/%Y')

CPI_adjusted <- adjusted_total[,c('Time', 'CPI_adjusted_GR', 'CPI_adjusted_PR')]
CPIC_adjusted <- adjusted_total[,c('Time', 'CPIC_adjusted_GR', 'CPIC_adjusted_PR')]
CPIC_adjusted <- na.omit(CPIC_adjusted)
PPI_adjusted <- adjusted_total[,c('Time', 'PPI_adjusted_GR', 'PPI_adjusted_PR')]
PCE_adjusted <- adjusted_total[,c('Time', 'PCE_adjusted_GR', 'PCE_adjusted_PR')]
PCE_adjusted <- na.omit(PCE_adjusted)
RS_adjusted <- rs[,c('Time', 'CPI_adjusted_Return')]


Cal_Return <- function(df, rename, con = 500){
  n <- dim(df)[1]
  r <- 0
  poname <- paste0(rename, '_Return')
  for (i in 1:n){
    r <- (con+r)*(1+df[i,rename])
    df[i,poname] <- r
  }
  df$type = "True Value"
  return(df)
}

# Cal_Return(CPI_adjusted, "CPI_adjusted_GR")

Sim_Return <- function(df, rename, simfun){
  n <- dim(df)[1]
  sim <- simfun(df, rename)
  sim_return <- Cal_Return(sim, rename)
  real_return <- Cal_Return(df, rename)
  poname <- paste0(rename, '_Return')
  if (sim_return[n,poname]>=real_return[n,poname]){
    sim_return$type = "Simulated Value (>= True Value)"
  } else {
    sim_return$type = "Simulated Value (< True Value)"
  }
  return(sim_return)
}

Sim_Return_Plot <- function(df, rename, iter, startfrom = 'Jan 1970', simfun){
  Return <- Cal_Return(df, rename)
  Return <- Return[Return$Time >= startfrom,]
  colnames(Return)[ncol(Return)-1] <- 'Value'
  
  poname <- paste0(rename, '_Return')
  
  sp <- ggplot()
  num = 0
  for (i in 1:iter) {
    sim_Return <- Sim_Return(df, rename, simfun = simfun)
    sim_Return <- sim_Return[sim_Return$Time >= startfrom,]
    colnames(sim_Return)[ncol(sim_Return)-1] <- 'Value'
    sp <- sp + 
      geom_line(data = sim_Return, aes(x = Time, y = log(Value), color = type))
    if (sim_Return$type[1] == "Simulated Value (>= True Value)"){
      num = num+1
    }
  }
  
  sp + 
    geom_line(data = Return, aes(x = Time, y = log(Value), color = type)) +
    annotate("text", x=1995, y=20, label= paste0(num/iter*100, "% of simulated ending values excess the real value")) + 
    theme_bw() +
    theme(legend.position="top",
          legend.title = element_blank()) +
    labs(ylab = "Log-Scale Portfolio Value")
}


# Simple Bootstraping

Spl_Boot <- function(df, rename){
  n <- dim(df)[1]
  df[,rename] = sample(df[,rename],n,replace=TRUE)
  return(df)
}


# Block Bootstraping

Blk_Boot <- function(df, rename, lstart = 3, lend = 18){
  n <- dim(df)[1]
  
  # random length of block
  lnum <- sample(lstart:lend,n/lstart,replace=TRUE)
  l <- cumsum(c(0,lnum))
  l <- c(l[l < n], n)
  
  for (j in seq_along(l[-1])){
    df[(l[j]+1):l[j+1],rename] <- sample(df[l[j]:l[j+1],rename],l[j+1]-l[j],replace=TRUE)
  }
  return(df)
}


# LogNormal

LN_Sim <- function(df, rename){
  n <- dim(df)[1]
  R_i <- log(df[,rename] + 1)
  R_bar <- mean(R_i)
  s2 <- sum((R_i-R_bar)^2)/(n-1)
  sigma <- sqrt(s2)
  mu <- R_bar+s2/2
  Z <- rnorm(n)
  df[,rename] <- exp((mu-1/2*sigma^2)+sigma*Z) - 1
  df
}
