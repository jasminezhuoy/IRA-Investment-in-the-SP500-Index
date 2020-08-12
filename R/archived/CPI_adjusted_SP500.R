library(dplyr)
## sp: 1926/01/30-2019/12/31, monthly
sp <- read.csv("./datasets/sp500 returns.csv")
sp <- sp[-1,1:3]
colnames(sp) <- c("sp.t", "GrossReturn", "PriceReturn")
# n <- dim(sp)[1]
# for (i in 1:n){
#   sp$cumGR[i] <- prod(1+sp$GrossReturn[i:n])-1
#   sp$cumPR[i] <- prod(1+sp$PriceReturn[i:n])-1
# }

## inf.cpi: 1913 Jan-2020 Jun, monthly
inf.cpi <- read.csv("./datasets/Inflation-CPI_clean.csv")
# cpi.20h1 <- mean(inf.cpi$CPI[inf.cpi$Year==2020][1:6])
n.cpi <- dim(inf.cpi)[1]
inf.cpi <- cbind(inf.cpi[-1,], CPI_pre = inf.cpi$CPI[-n.cpi])
inf.cpi <- inf.cpi %>% mutate(rate = (CPI - CPI_pre)/CPI_pre)
# inf.cpi <- inf.cpi %>% mutate(rate = (cpi.20h1 - CPI)/CPI) # based on 2020 value
# filter from 1926 to 2019
inf.cpi <- inf.cpi[inf.cpi$Year>=1926&inf.cpi$Year<=2019,c(1,2,3,5)]

## combine
total <- cbind(sp,inf.cpi)
total <- total %>% 
  mutate(adjusted.GR = (1+GrossReturn)/(1+rate)-1) %>% 
  mutate(adjusted.PR = (1+PriceReturn)/(1+rate)-1)
write.csv(total[,c(4,5,2,3,6,7,8,9)], file="./datasets/CPI-adjussted.csv")


# total <- cbind(sp,inf.cpi)
# total <- total %>% 
#   mutate(adjusted.cumGR = (1+cumGR)/(1+rate)-1) %>% 
#   mutate(adjusted.cumPR = (1+cumPR)/(1+rate)-1)
# total$adjusted.GR <- c(total$adjusted.cumGR[-1],0)
# total$adjusted.PR <- c(total$adjusted.cumPR[-1],0)
# total <- total %>% 
#   mutate(adjusted.GR = (1+adjusted.cumGR)/(1+adjusted.GR)-1) %>% 
#   mutate(adjusted.PR = (1+adjusted.cumPR)/(1+adjusted.PR)-1)
# write.csv(total[,c(4,5,2,3,6,7,8,9,10)], file="./datasets/CPI-adjussted.csv")
