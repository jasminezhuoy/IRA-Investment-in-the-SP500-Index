library(zoo)
library(ggplot2)
library(dplyr)

sp <- read.csv("./datasets/sp500 returns.csv")
sp <- sp[-1,1:3]
colnames(sp) <- c("Date", "GrossReturn", "PriceReturn")
sp <- data.frame(Year = as.integer(substr(sp$Date,1,4)),
                 Month = as.integer(substr(sp$Date,5,6)),
                 GrossReturn = sp$GrossReturn,
                 PriceReturn = sp$PriceReturn)
sp$Time <- as.yearmon(paste0(sp$Month, '/', sp$Year), format = '%m/%Y')

Cal_Port <- function(return, con = 500){
  n <- length(return)
  r <- 0
  portfolio <- c()
  for (i in 1:n){
    r <- (con+r)*(1+return[i])
    portfolio[i] <- r
  }
  return(portfolio)
}

Sim_Port <- function(return, year, iter = 100000, simfun){
  port <- list()
  for (i in 1:iter){
    re <- simfun(return, year = year)
    port[[i]] <- Cal_Port(re)
  }
  return(port)
}

# port <- Sim_Port(sp$GrossReturn, 40)

Cal_Ending <- function(port, year){
  iter <- length(port)
  e <- c()
  for (i in 1:iter){
    p <- port[[i]]
    e[i] <- p[year*12]
  }
  return(e)
}

# ending <- Cal_Ending(LNport, 40)

Cal_Quan <- function(port, ending, year){
  o <- order(ending)
  plotdata <- data.frame()
  t <- (1:(year*12))/12
  iter <- length(port)
  for (per in c(0.99,0.9,0.75,0.5,0.25,0.1,0.01)){
    plotdata <- rbind(plotdata, data.frame(Time = t,
                                           Portfolio = port[[o[per*iter]]][1:(year*12)], 
                                           Percentile = paste0(per*100, "% Percentile")))
  }
  return(plotdata)
}

# plotdata <- Cal_Quan(port, ending, 40)

Sim_Plot <- function(port, year){
  ending <- Cal_Ending(port, year)
  plotdata <- Cal_Quan(port, ending, year)
  
  m <- ceiling(quantile(ending,0.99)/1000000)*1000000
  
  ggplot() +
    geom_line(data=plotdata, aes(x = Time, y = Portfolio, color = Percentile)) +
    theme_bw() +
    labs(x = "Time in Years", y = "Portfolio Value")
}

Table_Quan <- function(port, year = c(20,25,30,35,40)){
  p <- data.frame()
  for (y in year){
    ending <- Cal_Ending(port, y)
    plotdata <- Cal_Quan(port, ending, y)
    pt <- plotdata %>% filter(Time==y)
    rownames(pt) <- pt$Percentile
    pt <- pt[2]
    colnames(pt) <- paste0(y, "-Year Portfolio")
    p <- rbind(p, t(pt))
  }
  t(p)
}


# Simple Bootstraping

Spl_Boot <- function(return, year = 40){
  n <- year * 12
  r <- sample(return,n,replace=TRUE)
  return(r)
}

# LogNormal

LN_Sim <- function(return, year = 40){
  n <- length(return)
  R_i <- log(return + 1)
  R_bar <- mean(R_i)
  s2 <- sum((R_i-R_bar)^2)/(n-1)
  sigma <- sqrt(s2)
  mu <- R_bar+s2/2
  Z <- rnorm(year*12)
  r <- exp((mu-1/2*sigma^2)+sigma*Z) - 1
  return(r)
}
