library(zoo)
library(ggplot2)
library(ggrepel)
library(dplyr)
# remotes::install_github("jonathan-g/gt")
library(gt)

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

Sim_Port <- function(return, year, iter = 10000, simfun){
  port <- list()
  for (i in 1:iter){
    re <- simfun(return, year = year)
    port[[i]] <- Cal_Port(re)
  }
  dp <- data.frame(matrix(unlist(port),nrow = iter, ncol=year*12,byrow = T))
  colnames(dp) <- paste0("month ", 1:(year*12))
  return(dp)
}

# port <- Sim_Port(sp$GrossReturn, 40)

# Cal_Ending <- function(port, year){
#   iter <- length(port)
#   e <- c()
#   for (i in 1:iter){
#     p <- port[[i]]
#     e[i] <- p[year*12]
#   }
#   return(e)
# }

# ending <- Cal_Ending(LNport, 40)

Cal_Quan <- function(dp, year){
  s <- data.frame(sapply(dp,sort))
  t <- (1:(year*12))/12
  iter <- nrow(dp)
  plotdata <- data.frame(Time = t,
                         Portfolio = as.vector(t(s[iter,1:(year*12)])),
                         Percentile = "Best")
  for (per in c(0.99,0.9,0.75,0.5,0.25,0.1,0.01)){
    plotdata <- rbind(plotdata, data.frame(Time = t,
                                           Portfolio = as.vector(t(s[per*iter,1:(year*12)])), 
                                           Percentile = paste0(per*100, "% Percentile")))
  }
  plotdata <- rbind(plotdata, data.frame(Time = t,
                                         Portfolio = as.vector(t(s[1,1:(year*12)])),
                                         Percentile = "Worst"))
  return(plotdata)
}

# plotdata <- Cal_Quan(port, ending, 40)

Sim_Plot <- function(dp, year){
  # ending <- Cal_Ending(port, year)
  plotdata <- Cal_Quan(dp, year)
  max_plotdata <- plotdata %>% 
    group_by(Percentile) %>% 
    filter(Portfolio<2000000) %>% 
    mutate(max = max(Portfolio)) %>%
    filter(Portfolio == max)
  label_data <- plotdata %>% 
    left_join(max_plotdata, by="Percentile") %>%
    filter(round(Time.x,2)==round(Time.y-2.5,2)) %>%
    mutate(angle = atan((Portfolio.y-Portfolio.x)/125000)*180/pi)
  
  ggplot() +
    geom_line(data=plotdata, aes(x = Time, y = Portfolio, color = Percentile), size=0.7) +
    geom_hline(yintercept = 1000000, linetype="dashed", size=0.7) +
    geom_text(data=label_data, aes(x=Time.x-1, y=Portfolio.x+100000, color = Percentile, label = Percentile),angle=label_data$angle) +
    theme_bw() +
    scale_y_continuous(limits = c(0,2000000),breaks=c(0,500000,1000000,1500000,2000000),labels = c("$0","$500,000", "$1,000,000", "$1,500,000","$2,000,000"), expand = c(0,0)) +
    scale_x_continuous(breaks = seq(0,year,5),expand = c(0,0)) +
    labs(x = "Years", y = "Portfolio Value")+
    theme(legend.box = "horizontal",legend.position = "none", 
          legend.title = element_blank(), panel.border = element_blank(),
          line = element_line(size=0.7), 
          axis.line = element_line(), axis.text = element_text(face='bold', size = 12),
          axis.title = element_text(size = 15),
          panel.grid.major =element_blank(), panel.grid.minor = element_blank())
}

Table_Quan <- function(dp, year = c(20,25,30,35,40,45)){
  p <- data.frame()
  s <- data.frame(sapply(dp,sort))
  for (y in year){
    t <- (1:(y*12))/12
    iter <- nrow(dp)
    pt <- data.frame(Portfolio = s[iter,y*12],
                     Percentile = "Best")
    for (per in c(0.99,0.9,0.75,0.5,0.25,0.1,0.01)){
      pt <- rbind(pt, data.frame(Portfolio = s[per*iter,y*12], 
                                       Percentile = paste0(per*100, "% Percentile")))
    }
    pt <- rbind(pt, data.frame(Portfolio = s[1,y*12],
                                     Percentile = "Worst"))
    
    rownames(pt) <- pt$Percentile
    pt <- pt[1]
    colnames(pt) <- paste0(y, " years")
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

# port <- Sim_Port(sp$GrossReturn, 40, simfun = Spl_Boot)
# LNport <- Sim_Port(sp$GrossReturn, 40, simfun = LN_Sim)

Tab_Years <- function(dp, year, con=500){
  s <- data.frame(sapply(dp,sort))
  t <- (1:(year*12))/12
  iter <- nrow(dp)
  plotdata <- data.frame(as.vector(s[iter,1:(year*12)]))
  q <- c(0.99,0.9,0.75,0.5,0.25,0.1,0.01)
  for (per in q){
    plotdata <- rbind(plotdata, as.vector(t(s[per*iter,1:(year*12)])))
  }
  plotdata <- rbind(plotdata, as.vector(s[1,1:(year*12)]))
  
  tab <- data.frame()
  for (i in seq_along(c(0,q,1))){
    m <- sum(plotdata[i,]<1000000)+1
    invest <- m*con
    y <- (m)/12
    if (y>year){
      y <- NA
      invest <- NA
    }
    tab <- rbind(tab,cbind(y,invest))
  }
  colnames(tab) <- c('Time', 'Invested')
  rownames(tab) <- c('Best',paste0(q*100, "{\\%}"),'Worst')
  
  return(tab)
}

Likelihood <- function(dp, year = c(20,25,30,35,40,45)){
  s <- data.frame(sapply(dp,sort))
  n <- dim(s)[1]
  like <- data.frame()
  for (y in year){
    yn <- s[,y*12]
    l <- sum(yn>=1000000)/n
    like <- rbind(like,data.frame(paste0(l*100,"%")))
  }
  like <- data.frame(t(like))
  colnames(like) <- paste0(year, " years")
  # rownames(like) <- "Likelihood of Reaching 1M"
  like$name <- "Likelihood of Reaching $\\text{\\$}$1M"
  return(like)
}

# write.csv(t(plotdata), file="./datasets/LNsimulation.csv")

# dp <- data.frame(matrix(unlist(port[1:10000]),10000,480,byrow = T))
# colnames(dp) <- paste0("month ", 1:480)
# dl <- data.frame(matrix(unlist(LNport[1:10000]),10000,480,byrow = T))
# colnames(dl) <- paste0("month ", 1:480)
# 
# write.csv(dp, file="./datasets/BSsimulation10k.csv", row.names = F)
# write.csv(dl, file="./datasets/LNsimulation10k.csv", row.names = F)

Sim_Table <- function(tabletotle,title){
  gt(data = tabletotal, rowname_col = "Percentile") %>%
    tab_header(
      title = "S&P 500 Return Simulation Results",
      subtitle = title
    ) %>%
    tab_spanner(
      label = "Portfolio Value After:",
      columns = vars(`20 years`,`25 years`, `30 years`, `35 years`, `40 years`, `45 years`)
    ) %>%
    tab_spanner(
      label = "To Reach $1M:",
      columns = vars(`Time`, `Invested`)
    ) %>%
    fmt_currency(
      columns = vars(`20 years`,`25 years`, `30 years`, `35 years`, `40 years`, `45 years`, `Invested`),
      currency = "USD",
      decimals = 0,
      suffixing = "k"
    ) 
  # for (i in c(20,25,30,35,40,45)){
  #   t %>%
  #     tab_style(
  #       style = cell_fill(color = "lightgreen"),
  #       locations = cells_body(
  #         columns = paste0(i, " years"),
  #         rows = as.symbol(paste0(i, " years"))>= 1000000
  #       )
  #     ) 
  # }
    
}


