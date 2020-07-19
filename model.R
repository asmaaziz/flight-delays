library(DAAG)
library(lattice)
 
#Loading the csv file
dfAll <- read.csv("DelayedFlights.csv", na.strings=c(""," ","NA"))
dfAll <- na.omit(dfAll)
#Sampling for plotting purposes
df1000 <- dfAll[sample(nrow(dfAll), 1000), ]
df <- dfAll[sample(nrow(dfAll), 300), ]
summary(df)
 
#changing time in hhmm format to minutes
df$DepTime <- floor(df$DepTime/100)*60 + (df$DepTime%%100)
df$CRSDepTime <- floor(df$CRSDepTime/100)*60+ (df$CRSDepTime%%100)
df$ArrTime = floor(df$ArrTime/100)*60+ (df$ArrTime%%100)
df$CRSArrTime = floor(df$CRSArrTime/100)*60+ (df$CRSArrTime%%100)
df$Month <- as.factor(df$Month)
df$DayOfWeek <- as.factor(df$DayOfWeek)
df$UniqueCarrier <- as.factor(df$UniqueCarrier)
df$Origin <- as.factor(df$Origin)
df$Dest <- as.factor(df$Dest)
dim(df)
 
boxplot(df$ArrDelay ~ df$DayOfWeek, data=df, horizontal = TRUE, col = "turquoise1",
    	main= "Comparing Arrival Delay and Day of the week",
    	xlab="Arrival Delay (in minutes)",
    	ylab="DayOfWeek (1=Monday, ..., 7=Sunday")
 
aggregate(dfAll[, "DepDelay"], list(dfAll$DayOfWeek), mean)
 
boxplot(df$ArrDelay ~ df$Month, data=df, horizontal = TRUE, col = "palevioletred2",
    	main= "Comparing Arrival Delay and Month",
    	xlab="Arrival Delay (in minutes)",
    	ylab="Month")
 
boxplot(df$ArrDelay ~ df$UniqueCarrier, data=df, horizontal = TRUE, col = "maroon4",
  	  main= "Comparing Arrival Delay and Unique Carrier",
    	xlab="Arrival Delay (in minutes)",
    	ylab="UniqueCarrier")
 
plot(df$DepDelay ~ df$TaxiOut)
 
aggregate(df[, "DepDelay"], list(df$Month), mean)
 
df$Month <- as.factor(df$Month)
summary(df$Month)
 
pairs(df[,c("ArrDelay", "DepDelay", "TaxiIn", "TaxiOut",
        	"AirTime", "Distance","CarrierDelay","WeatherDelay",
        	"NASDelay", "LateAircraftDelay")])
 
df$Month <- as.numeric(df$Month)
df$DayOfWeek <- as.numeric(df$DayOfWeek)
##Create a scatterplot
plot(DepDelay ~ Month, data = df, pch = 15, col = "palevioletred2")
panel.smooth(df$DepDelay, df$Month) 
plot(df, main = " Dep Delay as per Month")
abline(lm(DepDelay ~ Month, data = df))
panel.smooth(df$DepDelay, df$Month)
 
dfAll$DepTime <- floor(dfAll$DepTime/100)*60 + (dfAll$DepTime%%100)
dfAll$CRSDepTime <- floor(dfAll$CRSDepTime/100)*60+ (dfAll$CRSDepTime%%100)
dfAll$ArrTime = floor(dfAll$ArrTime/100)*60+ (dfAll$ArrTime%%100)
dfAll$CRSArrTime = floor(dfAll$CRSArrTime/100)*60+ (dfAll$CRSArrTime%%100)
summary(dfAll[,c("Month", "DayOfWeek", "DepTime", "CRSDepTime",
              	"ArrTime", "CRSArrTime", "UniqueCarrier",
             	"ActualElapsedTime", "CRSElapsedTime",
              	"ArrDelay", "DepDelay", "TaxiIn", "TaxiOut",
             	"AirTime", "Distance","CarrierDelay","WeatherDelay",
             	"NASDelay", "SecurityDelay", "LateAircraftDelay")])
 
model1 <- lm(ArrDelay ~ DepDelay, data = df1000)
plot(ArrDelay ~ DepDelay, data = df1000, pch = 17,col = "maroon")
abline(model1)
 
model2 <- lm(ArrDelay ~ Distance, data = df1000)
plot(ArrDelay ~ Distance, data = df1000, pch = 17,col = "maroon")
abline(model2)
 
##linear regression model
model3 <- lm(ArrDelay ~ Distance, data = df1000)
model3
##
cook <- cooks.distance(model3)
outliers <- cook[cook > 0.02]
plot(cook, ylab = "Cook's Distance")
 
##
plot(ArrDelay ~ Distance, data = df1000)
abline(model3, col="green")
 
model4 <- lm(ArrDelay ~ Distance, data = df1000[-outliers, ])
abline(model4, lty = 2, col="red")
#
plot(ArrDelay ~ Distance, data = df1000)
abline(model3)
abline(model4, lty = 2, col="red")
 
##Testing and Training data set
df1000$DayOfWeek <-  as.factor(df1000$DayOfWeek)
df1000$Month <-  as.factor(df1000$Month)
training.rows <-sample(1:nrow(df1000), size = 800)
 
df1000.train <- df1000[training.rows, ]
df1000.test <- df1000[-training.rows, ]
#Fit a linear regression model to predict  using all of the other variables on the training data.
 
 
dfAll$DayOfWeek <-  as.factor(dfAll$DayOfWeek)
dfAll$Month <-  as.factor(dfAll$Month)
training.rows <-sample(1:nrow(dfAll), size = floor(nrow(dfAll)*0.8))
 
dfAll.train <- dfAll[training.rows, ]
dfAll.test <- dfAll[-training.rows, ]
#Fit a linear regression model to predict  using all of the other variables on the training data.
dfAll.model1 <- lm(ArrDelay ~
                  	Month+ DayOfWeek+ DepTime+
                  	ArrTime+
                  	ActualElapsedTime+
                  	DepDelay+ TaxiIn+ TaxiOut+
                  	WeatherDelay+
                      NASDelay+ SecurityDelay,
                	data = dfAll.train)
summary(dfAll.model1)
 
dfAll.predict.train <- predict(dfAll.model1, newdata = dfAll.train)
sqrt(mean(dfAll.predict.train - dfAll.train$ArrDelay))
 
dfAll.predict.test <- predict(dfAll.model1, newdata = dfAll.test)
sqrt(mean(dfAll.predict.test - dfAll.test$ArrDelay))
 
keeps <- c("Month", "DayOfWeek", "DepTime", "CRSDepTime",
                  	"ArrTime", "CRSArrTime",
       	"ActualElapsedTime", "CRSElapsedTime",
 	      "DepDelay", "TaxiIn", "TaxiOut",
       	"Distance", "CarrierDelay", "WeatherDelay",
       	"NASDelay", "SecurityDelay")
