library(dplyr)
## sp: 1926/01/30-2019/12/31, monthly
sp <- read.csv("./datasets/sp500 returns.csv")
sp <- sp[-1,1:3]
colnames(sp) <- c("sp.t", "GrossReturn", "PriceReturn")

## inf.cpi: 1913 Jan-2020 Jun, monthly
inf.cpi <- read.csv("./datasets/Inflation-CPI_clean.csv")
cpi.20h1 <- mean(inf.cpi$CPI[inf.cpi$Year==2020][1:6])
inf.cpi <- inf.cpi %>% mutate(rate = (cpi.20h1 - CPI)/CPI) # based on 2020 value
# filter from 1926 to 2019
inf.cpi <- inf.cpi[inf.cpi$Year>=1926&inf.cpi$Year<=2019,]

## combine
total <- cbind(sp,inf.cpi)
total <- total %>% 
  mutate(adjusted.GR = (1+GrossReturn)/(1+rate)-1) %>% 
  mutate(adjusted.PR = (1+PriceReturn)/(1+rate)-1)
write.csv(total[,c(4,5,2,3,6,7,8)], file="./datasets/CPI-adjussted.csv")
