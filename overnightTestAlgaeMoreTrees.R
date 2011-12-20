
setwd("C:/Users/Alex/Documents/SYS 6018/PROJECT")
source("ROC.R")
library(randomForest)
source("randomForestM.R")


############################
###### DATA CLEANING #######
############################

library(DMwR)
# read in the data
algae.unclean <-read.table("algae.csv", sep=",", header=TRUE)

# convert size and speed to ordinal, drop the other 6 result variabls
algae.unclean$size <- NULL
algae.unclean$speed <- NULL
algae.unclean$season <- NULL

algae.unclean$a2 <- NULL
algae.unclean$a3 <- NULL
algae.unclean$a4 <- NULL
algae.unclean$a5 <- NULL
algae.unclean$a6 <- NULL
algae.unclean$a7 <- NULL

# which observations are missing 20% or more?
rowsToDelete <- manyNAs(algae.unclean, .2)
# Remove them
algae.unclean <- algae.unclean[-rowsToDelete,]

# Use 10-nn to impute other missing values
algae.clean <- knnImputation(algae.unclean, k=10, meth = "median")

# ALSO NEED TO CLEAN THE TEST DATA
algae.test <- read.table("algae.test.csv", sep=",", header=TRUE)
# join with algae test response
algae.testresponse <- read.table("algae.test.response.csv", sep=",", header=TRUE)
algae.test$a1 <- algae.testresponse$a1
#fix the columns as done to the training data
algae.test$size <- NULL
algae.test$speed <- NULL
algae.test$season <- NULL
#rowsToDelete <- manyNAs(algae.test, .2)
#algae.test <- algae.test[-rowsToDelete,]
algae.test <- knnImputation(algae.test, k=10, meth = "median")

#turn a1 into a factor
algae.data <- algae.clean
for (i in c(1:nrow(algae.data))) {
	if (algae.data$a1[i] > 0) algae.data$a1[i]=1
}
for (i in c(1:nrow(algae.test))) {
	if (algae.test$a1[i] > 0) algae.test$a1[i]=1
}
algae.data$a1 <- as.factor(algae.data$a1)
algae.test$a1 <- as.factor(algae.test$a1)


############################
  ###### BASELINE #######
############################
# choose 35 trees

algae.oldrf <- randomForest(a1 ~., data = algae.data, ntree = 35, mtry=1)
algae.rfpred <- predict(algae.oldrf, newdata = algae.test, type = "prob")[,2]

# build a tree for each value of M
algae.oldrf1 <- randomForest(a1 ~., data = algae.data, ntree = 35, mtry=1)
algae.rfpred1 <- predict(algae.oldrf1, newdata = algae.test, type = "response")
algae.oldrf2 <- randomForest(a1 ~., data = algae.data, ntree = 35, mtry=2)
algae.rfpred2 <- predict(algae.oldrf2, newdata = algae.test, type = "response")
algae.oldrf3 <- randomForest(a1 ~., data = algae.data, ntree = 35, mtry=3)
algae.rfpred3 <- predict(algae.oldrf3, newdata = algae.test, type = "response")
algae.oldrf4 <- randomForest(a1 ~., data = algae.data, ntree = 35, mtry=4)
algae.rfpred4 <- predict(algae.oldrf4, newdata = algae.test, type = "response")
algae.oldrf5 <- randomForest(a1 ~., data = algae.data, ntree = 35, mtry=5)
algae.rfpred5 <- predict(algae.oldrf5, newdata = algae.test, type = "response")
algae.oldrf6 <- randomForest(a1 ~., data = algae.data, ntree = 35, mtry=6)
algae.rfpred6 <- predict(algae.oldrf6, newdata = algae.test, type = "response")
algae.oldrf7 <- randomForest(a1 ~., data = algae.data, ntree = 35, mtry=7)
algae.rfpred7 <- predict(algae.oldrf7, newdata = algae.test, type = "response")
algae.oldrf8 <- randomForest(a1 ~., data = algae.data, ntree = 35, mtry=8)
algae.rfpred8 <- predict(algae.oldrf8, newdata = algae.test, type = "response")

#calculate the percentage accuracy
table1 <- table(algae.rfpred1, algae.test$a1)
percent_correct1 <- (table1[1,1]+table1[2,2]) / (sum(table1))
table2 <- table(algae.rfpred2, algae.test$a1)
percent_correct2 <- (table2[1,1]+table2[2,2]) / (sum(table2))
table3 <- table(algae.rfpred3, algae.test$a1)
percent_correct3 <- (table3[1,1]+table3[2,2]) / (sum(table3))
table4 <- table(algae.rfpred4, algae.test$a1)
percent_correct4 <- (table4[1,1]+table4[2,2]) / (sum(table4))
table5 <- table(algae.rfpred5, algae.test$a1)
percent_correct5 <- (table5[1,1]+table5[2,2]) / (sum(table5))
table6 <- table(algae.rfpred6, algae.test$a1)
percent_correct6 <- (table6[1,1]+table6[2,2]) / (sum(table6))
table7 <- table(algae.rfpred7, algae.test$a1)
percent_correct7 <- (table7[1,1]+table7[2,2]) / (sum(table7))
table8 <- table(algae.rfpred8, algae.test$a1)
percent_correct8 <- (table8[1,1]+table8[2,2]) / (sum(table8))

#write it to a file
output <- sprintf("percent correct algae, m=1 is %f \n",percent_correct1)
write(output, file="ALGAE_BASELINE_OUT.txt", append=T)
output <- sprintf("percent correct algae, m=2 is %f \n",percent_correct2)
write(output, file="ALGAE_BASELINE_OUT.txt", append=T)
output <- sprintf("percent correct algae, m=3 is %f \n",percent_correct3)
write(output, file="ALGAE_BASELINE_OUT.txt", append=T)
output <- sprintf("percent correct algae, m=4 is %f \n",percent_correct4)
write(output, file="ALGAE_BASELINE_OUT.txt", append=T)
output <- sprintf("percent correct algae, m=5 is %f \n",percent_correct5)
write(output, file="ALGAE_BASELINE_OUT.txt", append=T)
output <- sprintf("percent correct algae, m=6 is %f \n",percent_correct6)
write(output, file="ALGAE_BASELINE_OUT.txt", append=T)
output <- sprintf("percent correct algae, m=7 is %f \n",percent_correct7)
write(output, file="ALGAE_BASELINE_OUT.txt", append=T)
output <- sprintf("percent correct algae, m=8 is %f \n",percent_correct8)
write(output, file="ALGAE_BASELINE_OUT.txt", append=T)


#we'll use these results and go with m=4
algae.rfpred4 <- predict(algae.oldrf4, newdata = algae.test, type = "prob")[,2]
algae.oldRFpred <- algae.rfpred4


#baseline my RF code with 35 trees and 4 variables

algae.newRF <- randomForestM(algae.data,35,-4)
algae.newRFpred <- rf_predict(algae.newRF, algae.test)

jpeg(file="ALGAE_baselineplot_m4.jpg")
	plot.roc(algae.oldRFpred, algae.test$a1, col = "blue")
	lines.roc(algae.newRFpred,algae.test$a1, col = "red")
	leg.txt <- c("Original RF, M=4","My RF, M=4")
	legend("bottomright", leg.txt, col=c("blue","red"), lty=c(1, 1), cex=0.8, inset=0.02)
dev.off()


###########################################################
############################
  ###### TESTING #######
############################
###################################################################


### method 1 ###

algae.newRF1 <- randomForestM(algae.data, 35, 1)
algae.newRFpred1 <- rf_predict(algae.newRF1, algae.test)

algae.newRF1a <- randomForestM(algae.data, 70, 1)
algae.newRFpred1a <- rf_predict(algae.newRF1a, algae.test)
	
jpeg(file="ALGAE_method1trees.jpg")
	plot.roc(algae.oldRFpred, algae.test$a1, col = "blue")
	lines.roc(algae.newRFpred,algae.test$a1, col = "red")
	lines.roc(algae.newRFpred1, algae.test$a1, col = "green")
	lines.roc(algae.newRFpred1a, algae.test$a1, col = "black")
	leg.txt <- c("Original RF, M=4", "My RF, M=4", "METHOD 1", "METHOD 1 2x TREES")
	legend("bottomright", leg.txt, col=c("blue","red","green","black"), lty=c(1, 1, 1, 1), cex=0.8, inset=0.02)
dev.off()



###################################################################


### method 2 ###

algae.newRF2 <- randomForestM(algae.data, 35, 2)
algae.newRFpred2 <- rf_predict(algae.newRF2, algae.test)

algae.newRF2a <- randomForestM(algae.data, 70, 2)
algae.newRFpred2a <- rf_predict(algae.newRF2a, algae.test)
	
jpeg(file="ALGAE_method2trees.jpg")
	plot.roc(algae.oldRFpred, algae.test$a1, col = "blue")
	lines.roc(algae.newRFpred,algae.test$a1, col = "red")
	lines.roc(algae.newRFpred2, algae.test$a1, col = "green")
	lines.roc(algae.newRFpred2a, algae.test$a1, col = "black")
	leg.txt <- c("Original RF, M=4", "My RF, M=4", "METHOD 2", "METHOD 2 2x TREES")
	legend("bottomright", leg.txt, col=c("blue","red","green","black"), lty=c(1, 1, 1, 1), cex=0.8, inset=0.02)
dev.off()


###################################################################


### method 3 ###

algae.newRF3 <- randomForestM(algae.data, 35, 3)
algae.newRFpred3 <- rf_predict(algae.newRF3, algae.test)

algae.newRF3a <- randomForestM(algae.data, 70, 3)
algae.newRFpred3a <- rf_predict(algae.newRF1a, algae.test)
	
jpeg(file="ALGAE_method3trees.jpg")
	plot.roc(algae.oldRFpred, algae.test$a1, col = "blue")
	lines.roc(algae.newRFpred,algae.test$a1, col = "red")
	lines.roc(algae.newRFpred3, algae.test$a1, col = "green")
	lines.roc(algae.newRFpred3a, algae.test$a1, col = "black")
	leg.txt <- c("Original RF, M=4", "My RF, M=4", "METHOD 3", "METHOD 3 2x TREES")
	legend("bottomright", leg.txt, col=c("blue","red","green","black"), lty=c(1, 1, 1, 1), cex=0.8, inset=0.02)
dev.off()



###################################################################

### method 4 ###

algae.newRF4 <- randomForestM(algae.data, 35, 4)
algae.newRFpred4 <- rf_predict(algae.newRF4, algae.test)

algae.newRF4a <- randomForestM(algae.data, 70, 4)
algae.newRFpred4a <- rf_predict(algae.newRF4a, algae.test)
	
jpeg(file="ALGAE_method4trees.jpg")
	plot.roc(algae.oldRFpred, algae.test$a1, col = "blue")
	lines.roc(algae.newRFpred,algae.test$a1, col = "red")
	lines.roc(algae.newRFpred4, algae.test$a1, col = "green")
	lines.roc(algae.newRFpred4a, algae.test$a1, col = "black")
	leg.txt <- c("Original RF, M=4", "My RF, M=4", "METHOD 4", "METHOD 4 2x TREES")
	legend("bottomright", leg.txt, col=c("blue","red","green","black"), lty=c(1, 1, 1, 1), cex=0.8, inset=0.02)
dev.off()


###################################################################


### method 5 ###

algae.newRF5 <- randomForestM(algae.data, 35, 5)
algae.newRFpred5 <- rf_predict(algae.newRF5, algae.test)

algae.newRF5a <- randomForestM(algae.data, 70, 5)
algae.newRFpred5a <- rf_predict(algae.newRF5a, algae.test)
	
jpeg(file="ALGAE_method5trees.jpg")
	plot.roc(algae.oldRFpred, algae.test$a1, col = "blue")
	lines.roc(algae.newRFpred,algae.test$a1, col = "red")
	lines.roc(algae.newRFpred5, algae.test$a1, col = "green")
	lines.roc(algae.newRFpred5a, algae.test$a1, col = "black")
	leg.txt <- c("Original RF, M=4", "My RF, M=4", "METHOD 5", "METHOD 5 2x TREES")
	legend("bottomright", leg.txt, col=c("blue","red","green","black"), lty=c(1, 1, 1, 1), cex=0.8, inset=0.02)
dev.off()



###################################################################


### method 6 ###

algae.newRF6 <- randomForestM(algae.data, 35, 6)
algae.newRFpred6 <- rf_predict(algae.newRF6, algae.test)

algae.newRF6a <- randomForestM(algae.data, 70, 6)
algae.newRFpred6a <- rf_predict(algae.newRF5a, algae.test)
	
jpeg(file="ALGAE_method6trees.jpg")
	plot.roc(algae.oldRFpred, algae.test$a1, col = "blue")
	lines.roc(algae.newRFpred,algae.test$a1, col = "red")
	lines.roc(algae.newRFpred6, algae.test$a1, col = "green")
	lines.roc(algae.newRFpred6a, algae.test$a1, col = "black")
	leg.txt <- c("Original RF, M=4", "My RF, M=4", "METHOD 6", "METHOD 6 2x TREES")
	legend("bottomright", leg.txt, col=c("blue","red","green","black"), lty=c(1, 1, 1, 1), cex=0.8, inset=0.02)
dev.off()


###################################################################


