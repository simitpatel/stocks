library(plyr)
library(dplyr)
library(ggplot2)

#load data

may.insiders <- read.csv("./SA/insider1.csv")
buy.info <- read.csv("./SA/buys-fundamental.csv")
sells.info <- read.csv("./SA/sells-fundamental.csv")

#split by buys and sells

may.insiders.buys <- filter(may.insiders,Buy.Sell=="Buy")
may.insiders.sells <- filter(may.insiders,Buy.Sell=="Sell")

#group by company, sum amount spent

buys.grouped <- may.insiders.buys %>% group_by(Company) %>% summarise(sum(Cost))
sells.grouped <- may.insiders.sells %>% group_by(Company) %>% summarise(sum(Cost))

#rename and join

names(buys.grouped) <-c("Company","Buys.Cost")
names(sells.grouped) <-c("Company","Sell.Cost")
all.grouped <- merge(buys.grouped,sells.grouped,by="Company",all.x=TRUE,all.y=TRUE)
all.grouped <- merge(all.grouped,buy.info,by="Company",all.x=TRUE)
all.grouped <- merge(all.grouped,sells.info,by="Company",all.x=TRUE)
all.grouped <- all.grouped[,1:11]

#replace NA values with zero

all.grouped$Buys.Cost[is.na(all.grouped$Buys.Cost)] <- 0
all.grouped$Sell.Cost[is.na(all.grouped$Sell.Cost)] <- 0

#calculate percentages, remove invalid market cap

all.grouped <- mutate(all.grouped, Net.Buy.Pct = (Buys.Cost-Sell.Cost)/(Market.Cap.x*1000000))
all.grouped <- all.grouped[!is.na(all.grouped$Market.Cap.x),]

#find values

min.val <- all.grouped[which(all.grouped$Net.Buy.Pct == min(all.grouped$Net.Buy.Pct)), ]
max.val <- all.grouped[which(all.grouped$Net.Buy.Pct == max(all.grouped$Net.Buy.Pct)), ]
minus1 <- all.grouped[which(all.grouped$Net.Buy.Pct <= -0.1), ]
plus1 <- all.grouped[which(all.grouped$Net.Buy.Pct >= 0.01), ]

