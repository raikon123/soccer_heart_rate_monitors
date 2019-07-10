#clear the memory
rm (list = ls())

install.packages("randomForest")
install.packages("caTools")
install.packages("MASS")
install.packages("forecast")
install.packages("rpart")


library(forecast)
library(ggplot2)
library(rpart)	
library(readxl)
library(randomForest)
library(caTools)
library(rpart.plot)
library(rpart)

soccer <- read_xlsx("c:\\users\\njohnson1\\Desktop\\Heart_Rate_MU_Soccer.xlsx")
soccer


sc1 <- data.frame(soccer)
sc1

#remove variables
sc1$Player.number <- NULL
sc1$Player.name <- NULL
sc1$Phase.name <- NULL
sc1$Duration <- NULL
sc1$Start.time <- NULL
sc1$End.time <- NULL
sc1$Recovery.time..h. <- NULL
sc1$Training.load.score <- NULL
sc1$Time.in.HR.zone.1..50...59... <- NULL
sc1$Time.in.HR.zone.2..60...69... <- NULL
sc1$Time.in.HR.zone.3..70...79... <- NULL
sc1$Time.in.HR.zone.4..80...89... <- NULL
sc1$Time.in.HR.zone.5..90...100... <- NULL


#denote factors
sc1$Home.Away = as.factor(sc1$Home.Away)



#set seed
set.seed(123)


#train and test data split
split <- sample.split(sc1, SplitRatio=0.7)
training_set = subset(sc1, split == TRUE)
test_set = subset(sc1, split == FALSE)

str(training_set)
#variable with 100 trees
#Variable Maryville.socre goals
finalml100 <- randomForest(Maryville.Score ~ ., data = training_set, ntree = 100, na.action = na.exclude)
finalml100
plot(finalml100)
summary(finalml100)

#Variable Maryville.socre goals
finalml20 <- randomForest(Maryville.Score ~ ., data = training_set, ntree = 20, na.action = na.exclude)
finalml20
plot(finalml20)
summary(finalml20)

#random forest for finalml
print(finalml20)
importance(finalml20) #returns GINI
table(predict(finalml20), training_set$Maryville.Score)

plot(importance(finalml20))
varImpPlot(finalml20,type=2)
getTree(finalml20, k=1, labelVar=FALSE)

#rpart for Maryville.score
finalmodel <- rpart(Maryville.Score ~ ., data = training_set, na.action=na.exclude)

printcp(finalmodel)
summary(finalmodel)
plotcp(finalmodel)

plot(finalmodel, uniform = TRUE )
text(finalmodel, use.n = TRUE, all = TRUE, cex = .8)

#forecast
#load data for first half
soccerplot1 <- read_xlsx("c:\\users\\njohnson1\\Desktop\\Soccer_Cleaner_Final.xlsx", sheet = 6)
soccerplot1

soccerplotframe1 <- data.frame(soccerplot1)
soccerplotframe1

#forecast(auto.arima(soccerplotframe))
#plot
#goal 1
plot(soccerplotframe1$x7, soccerplotframe1$y7,  type = "l", col = "red", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe1$x6, soccerplotframe1$y6,  type = "l", col = "blue", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe1$x3, soccerplotframe1$y3,  type = "l", col = "green", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))

#goal 2
plot(soccerplotframe1$x6, soccerplotframe1$y6,  type = "l", col = "blue", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe1$x5, soccerplotframe1$y5,  type = "l", col = "gold", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))

#goal 3
plot(soccerplotframe1$x2, soccerplotframe1$y2,  type = "l", col = "deeppink", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe1$x4, soccerplotframe1$y4,  type = "l", col = "magenta", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))


#second half
soccerplot2 <- read_xlsx("c:\\users\\njohnson1\\Desktop\\Soccer_Cleaner_Final.xlsx", sheet = 5)
soccerplot2

soccerplotframe2 <- data.frame(soccerplot2)
soccerplotframe2

#goal 4
plot(soccerplotframe2$x2, soccerplotframe2$y2,  type = "l", col = "deeppink", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))

#goal 5
plot(soccerplotframe2$x1, soccerplotframe2$y1,  type = "l", col = "midnightblue", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))

#first half overlap
plot(soccerplotframe1$x7, soccerplotframe1$y7,  type = "l", col = "red", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe1$x6, soccerplotframe1$y6,  type = "l", col = "blue", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe1$x3, soccerplotframe1$y3,  type = "l", col = "green", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe1$x6, soccerplotframe1$y6,  type = "l", col = "blue", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe1$x5, soccerplotframe1$y5,  type = "l", col = "gold", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe1$x2, soccerplotframe1$y2,  type = "l", col = "deeppink", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe1$x4, soccerplotframe1$y4,  type = "l", col = "magenta", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))


#second half goal overlap
plot(soccerplotframe2$x2, soccerplotframe2$y2,  type = "l", col = "deeppink", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))
par(new=TRUE)
plot(soccerplotframe2$x1, soccerplotframe2$y1,  type = "l", col = "midnightblue", xaxt = "n", yaxt = "n", ylim=c(0,100), xlim=c(0,100))


#cordRF <- randomForest()



#Variable Opponent.socre goals
finalml1 <- randomForest(Opponent.Score ~ ., data = training_set, ntree = 20, na.action = na.exclude)
finalml1


#random forest for finalml1
print(finalml1)
importance(finalml1) #returns GINI
table(predict(finalml1), training_set$Maryville.Score)
plot(finalml1) 
plot(importance(finalml1))
varImpPlot(finalml1,type=2)
getTree(finalml1, k=1, labelVar=FALSE)

#maptree with CART
library(maptree) #part of maptree
library(cluster) #part of CART
draw.tree(clip.rpart(rpart(Maryville.Score ~ ., data = sc1), best = 7), nodeinfo = TRUE)

#rpart for Opponent.score
finalmodel1 <- rpart(Opponent.Score ~ ., data = training_set, na.action=na.exclude)

printcp(finalmodel1)
summary(finalmodel1)
plotcp(finalmodel1)

plot(finalmodel1, uniform = TRUE )
text(finalmodel1, use.n = TRUE, all = TRUE, cex = .8)

