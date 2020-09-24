# Robert Shiller adjusted SP500: https://www.multpl.com/inflation-adjusted-s-p-500/table/by-month
# 1871-2020
library(dplyr)
library(readxl)
library(zoo)
rs <- read_xls("./datasets/RobertShiller.xls", sheet = 4, skip = 7)
rs <- data.frame(rs)
rs <- rs[,c(1,8,5)]
colnames(rs) <- c('Time', 'CPI_adjusted_Price','CPI')
rs$Year <- floor(rs$Time)
rs$Month <- (rs$Time*100)%%100
rs$Time <- as.yearmon(paste0(rs$Month, '/', rs$Year), format = '%m/%Y')
rs <- na.omit(rs)
rs <- rs[rs$Year>1926,]
n <- dim(rs)[1]

rs <- cbind(rs[-1,], Price_pre = rs$CPI_adjusted_Price[-n])
rs <- rs %>% mutate(CPI_adjusted_Return = (CPI_adjusted_Price - Price_pre)/Price_pre)
rs_CPI <- rs$CPI
rs <- rs[,c(1,4,5,2,7)]

# write.csv(rs, file="./datasets/Robert_Shiller_CPI_adjusted_Return.csv", row.names = F)
