require(ggplot2)
require(reshape2)
require(stringr)
require(gridExtra)

# Vmetnuvanje na Log file od Dejan

rssi1 <- read.table(file = "rssi1m.txt",fill = TRUE,stringsAsFactors=FALSE)
rssi2 <- read.table(file = "rssi2m.txt",fill = TRUE,stringsAsFactors=FALSE)

# head(rssi2)
# head(rssi1)

l <- rssi1[str_detect(rssi1$V12,pattern = "power:-\\d{1,3}"), ]
l1 <- rssi2[str_detect(rssi2$V12,pattern = "power:-\\d{1,3}"), ]

power_1m <- str_extract(string = l$V12 ,pattern = "-\\d{1,3}" )
power_2m <- str_extract(string = l1$V12 ,pattern = "-\\d{1,3}" )

# length(power_1m)
# head(power_1m)

power_1m <- as.data.frame(power_1m)
power_2m <- as.data.frame(power_2m)
power_1m$power_1m <- as.numeric(as.character(power_1m$power_1m))
power_2m$power_2m <- as.numeric(as.character(power_2m$power_2m))

A_1m <- mean(power_1m$power_1m,na.rm = TRUE)
A_2m <- mean(power_2m$power_2m,na.rm = TRUE)
A <- (A_1m + A_2m) / 2

# head(power_1m)
# class(power_1m$power_1m)

# Creating plots and geom_smooth(method="lm") , creating linear regression models

g <- ggplot(data = power_1m,aes(y = power_1m,x = 1:length(power_1m))) + geom_point(fill = "red") + geom_smooth(method="lm",color = "red") +
  labs( x = "Number of observations (1 metter distance)", y = "RSSI(dbm)",title = "Received RSSI signals")

g1 <- ggplot(data = power_2m,aes(y = power_2m,x = 1:length(power_2m))) + geom_point() + geom_smooth(method="lm",color = "red") +
    labs( x = "Number of observations (2 metter distance)", y = "RSSI(dbm)", title = "Received RSSI signals")

####################           Linear regression                           #############################
x1 <-  1:length(power_1m$power_1m)
x2 <-  1:length(power_2m$power_2m)

reg_1m <- lm(data = power_1m, power_1m ~ x1)
reg_2m <- lm(data = power_2m, power_2m ~ x2)

sum_1m <- summary(reg_1m)
sum_2m <- summary(reg_2m)

rez <- list(Distance_1m = reg_1m,Distance_2m = reg_2m)
########################################################################################################

power_1m$power_1m <- as.factor(as.character(power_1m$power_1m)) # Vrakjanje vo factor za podobar 
power_2m$power_2m <- as.factor(as.character(power_2m$power_2m)) # prikaz na x -oska             

hist_1m <- ggplot(data = power_1m,aes(x = power_1m)) + geom_histogram(colour = "black", fill = "darkgreen") +
  labs( x = "Different RSSI for 1m distance", y = "Frequency")     

hist_2m <- ggplot(data = power_2m,aes(x = power_2m)) + geom_histogram(colour = "black", fill = "darkgreen") +  
  labs( x = "Different RSSI for 2m distance", y = "Frequency")

grid.arrange(g, g1, hist_1m, hist_2m, nrow = 2,ncol = 2, main=textGrob("RSSI ANALYSIS", gp=gpar(cex=1.2, fontface="bold", col="blue")))

cat(sprintf(" \n Mean for 1m distance is %s. \n Mean for 2m distance is %s. \n Overal mean is %s. \n ",A_1m,A_2m,A)) # Printanje na srednata vrednost

################# Print reggresion rezults ###########################

sum_1m
# reg_1m
sum_2m
# reg_2m