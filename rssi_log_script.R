require(ggplot2)
require(reshape2)
require(stringr)

# Vmetnuvanje na Log file od Dejan

rssi1 <- read.table(file = "rssi1m.txt",fill = TRUE,stringsAsFactors=FALSE)
rssi2 <- read.table(file = "rssi2m.txt",fill = TRUE,stringsAsFactors=FALSE)
head(rssi1)
# head(rssi1)
# nrow(rssi1)
# str(rssi1)
# any(str_detect(rssi1$V14,pattern = "power:-\\d{1,3}"))

l <- rssi1[str_detect(rssi1$V12,pattern = "power:-\\d{1,3}"), ]
l1 <- rssi2[str_detect(rssi1$V12,pattern = "power:-\\d{1,3}"), ]
# head(l)
# nrow(l)
power_1m <- str_extract(string = l$V12 ,pattern = "-\\d{1,3}" )
power_2m <- str_extract(string = l1$V12 ,pattern = "-\\d{1,3}" )
# length(power)
# head(power)
# class(power)

power_1m<- as.data.frame(power_1m)
power_2m<- as.data.frame(power_2m)
head(power_1m)
head(power_2m)
g <- ggplot(data = power,aes(y = power,x = 1:length(power))) + geom_point(aes(fill = "l",color ="red")) + 
  labs( x = "Number of observations", y = "RSSI(dbm)",title = "Received RSSI signals")

hist <- ggplot(data = power,aes(x = power)) + geom_histogram()
hist
