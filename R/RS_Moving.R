source("R/Robert_Shiller_adjusted_Return.R")
source("R/inflation_adjusted_weighted_return.R")

rs_con <- 500/257.32*rs_CPI
rs_con <- rs_con[1:(length(rs_con)-6)]

Cal_Port <- function(return, con = rs_con){
  n <- length(return)
  con <- con[-c(1:n)]
  r <- 0
  portfolio <- c()
  for (i in 1:n){
    r <- (con[i]+r)*(1+return[i])
    portfolio[i] <- r
  }
  return(portfolio)
}

Port_Real_Year <- function(return, year){
  e <- c()
  N <- length(return)
  m <- year*12
  for (i in 1:(N-m+1)){
    e[i] <- Cal_Port(return[i:(i+m-1)])[m]
  }
  return(e)
}

rs_c <- 1000000/257.32*rs_CPI
rs_c <- rs_c[1:(length(rs_c)-6)]


Prop_over_1m <- function(return, year, thred=rs_c){
  p <- Port_Real_Year(return, year)
  mean(p>=thred[c(1:length(p))])
}

Port_Real_Year(sp$PriceReturn, 20)
Port_Real_Year(rs$CPI_adjusted_Return, 40)
Prop_over_1m(sp$GrossReturn, 45)
Prop_over_1m(rs$CPI_adjusted_Return, 35)
Prop_over_1m(adjusted_total$PPI_adjusted_GR, 25)
