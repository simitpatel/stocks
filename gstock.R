#load libraries

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(Quandl)

#read data

gstock1 <- read.csv("./SA/gstock060915.csv")
gstock22 <- read.csv("./SA/gstock-22.csv")
fnv1 <- Quandl("YAHOO/FNV", authcode="7kHc8PZxjLdhCz4C-RMG")
gld1 <- Quandl("WGC/GOLD_DAILY_USD", authcode="7kHc8PZxjLdhCz4C-RMG")
fnv.si <- Quandl("SI/FNV_SI", authcode="7kHc8PZxjLdhCz4C-RMG", 
                trim_start="2007-12-07", trim_end="2015-06-09")
fnv.income <- Quandl("ZFB/FNV_CONSOL_NET_INCOME_LOSS_Q", authcode="7kHc8PZxjLdhCz4C-RMG", 
       trim_start="2011-09-30")
fnv.lta <- Quandl("ZFA/FNV_TOT_LTERM_ASSET_Q", authcode="7kHc8PZxjLdhCz4C-RMG")
fnv.fcf <- Quandl("ZFA/FNV_FREE_CASH_FLOW_Q", authcode="7kHc8PZxjLdhCz4C-RMG")
fnv.se <- Quandl("ZFA/FNV_TOT_SHARE_HOLDER_EQUITY_Q", authcode="7kHc8PZxjLdhCz4C-RMG", 
                 trim_start="2011-09-29")
fnv.rev <- Quandl("ZFA/FNV_TOT_REVNU_Q", authcode="7kHc8PZxjLdhCz4C-RMG")


#rename columns to prep for joins

fnvall <- fnv1[,c(1,6,7)]
fnv.fcf <- rename(fnv.fcf,Date=PER_END_DATE)
fnv.income <- rename(fnv.income,Date=PER_END_DATE)
fnv.lta <- rename(fnv.lta,Date=PER_END_DATE)
fnv.se <- rename(fnv.se,Date=PER_END_DATE)
names(fnv.si) <- c("Date","Short.Interest","Avg.Daily","Days.Cover")
names(fnvall) <- c("Date","Volume","Price")

#left joins to fnvall

fnvall <- left_join(fnvall,fnv.fcf,by="Date")
fnvall <- left_join(fnvall,fnv.income,by="Date")
fnvall <- left_join(fnvall,fnv.lta,by="Date")
fnvall <- left_join(fnvall,fnv.se,by="Date")
fnvall <- left_join(fnvall,fnv.si,by="Date")

#create version with full values

fnvall2 <- na.omit(fnvall)
fnvall2 <- fnvall2[,c(1,3:7,10)]

#create set with just LTA, SE, and P

fnv2 <- fnv1[,c(1,6,7)]
fnv2 <- left_join(fnv2,fnv.lta,by="Date")
fnv2 <- left_join(fnv2,fnv.se,by="Date")
fnv2 <- na.omit(fnv2)
fnv2 <- mutate(fnv2, TOT_LTERM_ASSET=TOT_LTERM_ASSET/2)
fnv2 <- mutate(fnv2, TOT_SHARE_HOLDER_EQUITY=TOT_SHARE_HOLDER_EQUITY/100)
fnv2 <- fnv2[,c(1,3:5)]
names(fnv2) <- c("Date","Price","Assets","Equity")
fnv2.melt <- melt(fnv2,id="Date")

fnv2.melt.p <- ggplot(data=fnv2.melt, aes(x=Date, y=value, group=variable,color=variable)) +
  geom_line(size=2)

#create set for revenue and price
fnv3 <- fnv1[,c(1,6,7)]
fnv.rev <- rename(fnv.rev,Date=PER_END_DATE)
fnv3 <- left_join(fnv3,fnv.rev,by="Date")
fnv3 <- na.omit(fnv3)
fnv3 <- fnv3[,c(1,3,4)]

#mutate to make price ratios

fnvall <- mutate(fnvall,p.fcf = Price/FREE_CASH_FLOW)
fnvall <- mutate(fnvall,p.ni = Price/FREE_CASH_FLOW)

fnvall2 <- mutate(fnvall2, FREE_CASH_FLOW=FREE_CASH_FLOW/10)
fnvall2 <- mutate(fnvall2, TOT_LTERM_ASSET=TOT_LTERM_ASSET/2)
fnvall2 <- mutate(fnvall2, TOT_SHARE_HOLDER_EQUITY=TOT_SHARE_HOLDER_EQUITY/100)
fnvall2.melt <- melt(fnvall2,id="Date")

fnvall2.melt.p <- ggplot(data=fnvall2.melt, aes(x=Date, y=value, group=variable,color=variable)) +
       geom_line(size=2)

#compare gld to fnv price

fnv1a <- fnv1[,c(1,7)]
fnv1a <- fnv1a[1:1200,]
gld1 <- gld1[1:1200,]
gld1 <- mutate(gld1,Asset="Gold")
fnv1a <- mutate(fnv1a,Asset="FNV")
names(gld1) <- c("Date","Price","Asset")
names(fnv1a) <- c("Date","Price","Asset")
gld1 <- mutate(gld1,Price=Price/10)
gld1$Date <- as.character(gld1$Date)
fnv1a$Date <- as.character(fnv1a$Date)
fnvgld <- rbind(gld1,fnv1a,by="Date")
fnvgld <- filter(fnvgld,Asset!="Date")
fnvgld$Date <- as.Date(fnvgld$Date)
fnvgld.p <- ggplot(aes(x=Date,y=Price,color=Asset,group=Asset),data=fnvgld) + geom_line()

#price to book of gold stock industry

gstock1 <- filter(gstock1,Market.Cap>=1000)
gstock12 <- inner_join(gstock1,gstock22,by="Symbol")
gstock12.p <- ggplot(gstock12,aes(Symbol,PB,colour=Symbol,size=5)) + geom_point() + theme(legend.position="none")


