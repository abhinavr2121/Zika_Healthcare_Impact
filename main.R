library(ggplot2)
library(ggthemes)

# IMPORTANT VARIABLES
SHIFT <- 7 # window length/time interval for moving average

# READ DATA
zika <- read.csv('data/cdc_zika.csv', stringsAsFactors = F)
vht <- read.csv('data/VHT.csv', stringsAsFactors = F)

# DATA CLEANING
# SELECT ONLY US REPORTS
zika <- zika[substring(zika$location, 1, 13) == "United_States", ]
zika <- subset(zika, select = c("report_date", "value"))
zika$value <- as.numeric(zika$value)

# LUMP SUM BY DATE
zika <- aggregate(zika$value, by = list(report_date = zika$report_date), FUN = sum)
zika$report_date <- as.Date(zika$report_date, "%Y-%m-%d")

# SELECT ONLY VHT DATA FOR 2016
vht <- subset(vht, select = c("Date", "Close"))
vht <- vht[substring(vht$Date, 1, 4) == 2016, ]

# FIND CHANGE IN DAILY PRICE
dates <- c()
deltas <- c(0)
for(i in SHIFT : SHIFT : length(vht$Date)) {
  dates <- append(dates, vht$Date[i])
  deltas <- append(deltas, (vht$Close[i] - vht$Close[i - SHIFT]) / SHIFT)
}
dvht <- data.frame(dates = as.Date(dates, "%Y-%m-%d"), deltas)

# MERGE DATA FRAMES BY DATE
date <- c()
dt <- c()
counter <- 1
while(counter != length(zika$report_date)) {
  sub <- subset(dvht, dvht$dates == zika$report_date[counter])
  if(length(sub) != 0) {
    date <- append(date, sub$dates)
    dt <- append(dt, sub$deltas)
    counter <- counter + 1
  }
}

zika <- subset(zika, zika$report_date %in% date)
rdf <- data.frame(date, delta = dt, disease = zika$x)

# PERFORM LINEAR FIT
lin.model <- lm(delta ~ disease, rdf)
corr.coef <- sqrt(summary(lin.model)$r.squared[1]) * (lin.model$coefficients[2] / abs(lin.model$coefficients[2]))
corr.coef[[1]] # correlation coefficient (-0.2196974)

# FINAL GRAPH
p <- ggplot(rdf, aes(x = disease, y = delta)) + geom_point(size = 2)
p <- p + geom_abline(slope = lin.model$coefficients[[2]], intercept = lin.model$coefficients[[1]], col = 'red', size = 1.5)
p <- p + xlab("Disease Counts") + ylab("Change in VHT [$]") + ggtitle("Disease Count vs. Change in VHT Price (March 2016 - June 2016)")
p <- p + theme_hc()
p

# RESIDUALS
rdf$residuals <- lin.model$residuals
r <- ggplot(rdf, aes(x = disease, y = residuals)) + geom_point()
