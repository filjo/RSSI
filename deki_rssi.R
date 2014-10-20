require(ggplot2)
require(reshape2)
require(stringr)
require(gridExtra)

# Vmetnuvanje na Log file od Dejan

rssi1 <- read.table(file = "iPhone5_rssi0,5m.txt",fill = TRUE,stringsAsFactors=FALSE)
rssi2 <- read.table(file = "iPhone5_rssi1m.txt",fill = TRUE,stringsAsFactors=FALSE)
rssi3 <- read.table(file = "iPhone5_rssi2m.txt",fill = TRUE,stringsAsFactors=FALSE)

# head(rssi2)

power_0.5m <-rssi1[!is.na(rssi1$V13), ]
power_1m  <- rssi2[!is.na(rssi2$V13), ]
power_2m  <- rssi3[!is.na(rssi3$V13), ]

# power_0.5m <- subset.data.frame(power_0.5m, power_0.5m$V13 >= -53 & power_0.5m$V13 <= -51)
# power_1m <-   subset.data.frame(power_1m, power_1m$V13 >= -55 & power_1m$V13 <= -46)

# m1 <- ggplot(data = power_0.5m,aes( y = V13, x= 1 : length(V13))) + geom_boxplot(fill = "red")
# m2 <- ggplot(data = power_1m,aes( y = V13, x= 1 : length(V13))) + geom_boxplot(fill = "red") 
# m3 <- ggplot(data = power_2m,aes( y = V13, x= 1 : length(V13))) + geom_boxplot(fill = "red")

box_1 <- summary(power_0.5m$V13)
box_2 <- summary(power_1m$V13)
box_3 <- summary(power_2m$V13)

g1 <- ggplot(data = power_0.5m,aes( y = V13, x= 1 : length(V13))) + 
  geom_point(fill = "red") + geom_smooth(method="lm",color = "red") +
  labs( x = "Number of observations (0.5 metter distance)", y = "RSSI(dbm)",title = "Received RSSI signals") + scale_y_continuous(limits=c(-65,-40))

g2 <- ggplot(data = power_1m,aes( y = V13, x = 1 : length(V13))) +
   geom_point(fill = "red") + geom_smooth(method="lm",color = "red") +
  labs( x = "Number of observations (1 metter distance)", y = "RSSI(dbm)",title = "Received RSSI signals") + scale_y_continuous(limits=c(-70,-30))

g3 <- ggplot(data = power_2m,aes( y = V13, x= 1 : length(V13))) + 
  geom_point(fill = "red") + geom_smooth(method="lm",color = "red") +
  labs( x = "Number of observations (2 metter distance)", y = "RSSI(dbm)",title = "Received RSSI signals") + scale_y_continuous(limits=c(-80,-40))

# Linear regression values

x <- 1 : length(power_0.5m$V13)
lm_0.5 <- lm(data = power_0.5m,power_0.5m$V13 ~ x)
lm_1   <- lm(data = power_1m,power_1m$V13 ~ x)
lm_2   <- lm(data = power_2m,power_2m$V13 ~ x)

m1 <- ggplot(lm_0.5, aes(.fitted, .resid)) + geom_point() + geom_smooth(method = "lm") + labs(title="Explain the method") # fitted vs residuals
m2 <- ggplot(lm_1, aes(.fitted, .resid)) + geom_point() + geom_smooth(method = "lm") + labs(title="Explain the method")   # fitted vs residuals
m3 <- ggplot(lm_2, aes(.fitted, .resid)) + geom_point() + geom_smooth(method = "lm") + labs(title="Explain the method")   # fitted vs residuals

# Going back to factor to represent the data as histograms

power_0.5m$V13 <- as.factor(as.character(power_0.5m$V13))
power_1m$V13 <- as.factor(as.character(power_1m$V13))
power_2m$V13 <- as.factor(as.character(power_2m$V13))


hist_0.5m <- ggplot(data = power_0.5m,aes(x = V13)) + geom_histogram(colour = "black", fill = "darkgreen", binwidth = 0.7) +
  labs( x = "Different RSSI for 1m distance", y = "Frequency") 

hist_1m <- ggplot(data = power_1m,aes(x = V13)) + geom_histogram(colour = "black", fill = "darkgreen", binwidth = 1) +
  labs( x = "Different RSSI for 1m distance", y = "Frequency") 

hist_2m <- ggplot(data = power_2m,aes(x = V13)) + geom_histogram(colour = "black", fill = "darkgreen", binwidth = 1.5) +
  labs( x = "Different RSSI for 1m distance", y = "Frequency") 


grid.arrange(g1, g2, g3, m1, m2, m3 ,hist_0.5m, hist_1m, hist_2m, nrow =3,ncol = 3, 
             main=textGrob("RSSI ANALYSIS - IPhone5", gp=gpar(cex=1.2, fontface="bold", col="blue")))

cat(sprintf("\t Za 0.5m :minimalnata vrednost %s,
             Za 0.5m :srednata vrednost  %s
             Za 0.5m :maximalnata vrednost  %s",
            box_1[1],box_1[3],box_1[5]))
cat(sprintf("Za 1m :minimalnata vrednost %s
             Za 1m :srednata vrednost  %s
             Za 1m :maximalnata vrednost  %s",
            box_2[1],box_2[3],box_2[5]))
cat(sprintf("Za 2m :minimalnata vrednost %s
             Za 2m :srednata vrednostt  %s
             Za 2m :maximalnata vrednost  %s",
             box_3[1],box_3[3],box_3[5]))

summary(lm_0.5)
summary(lm_1)
summary(lm_2)

# After conclusion on the results, we need to use non - parametric method :D
