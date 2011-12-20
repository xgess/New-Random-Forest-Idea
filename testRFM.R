
setwd("C:/Users/Alex/Documents/SYS 6018/PROJECT")
spam.unclean <-read.table("spamSmall.txt", sep="	", header=FALSE)
keeps <- c("V50", "V51", "V52", "V53", "V54", "V55", "V56", "V57", "V58")
spam.data <- spam.unclean[keeps]
#spam.test <- read.table("spamTest.txt", sep="	", header=FALSE)
#spam.test <- spam.test[keeps]

spam.full <- read.table("Spam.txt", sep="	", header=FALSE)

#spam.full <- spam.full[keeps]


i <- sample(1:nrow(spam.full), round(0.33*nrow(spam.full)))
j <- setdiff(1:nrow(spam.full), i)
testData <- spam.full[i,]
trainData <- spam.full[j,]


source("randomForestM.R")
smallrf <- randomForestM(spam.data, 1, 6)





rf <- randomForestM(trainData,1)
prediction <- rf_predict(rf,testData)

#########################################################################
## pima test data
## 30 trees
## M=5 for the old method
#########################################################################

pima.full <- read.table("PimaData.txt", sep="	", header=TRUE)
i <- sample(1:nrow(pima.full), round(0.33*nrow(pima.full)))
j <- setdiff(1:nrow(pima.full), i)
pima.test <- pima.full[i,]
pima.train <- pima.full[j,]


## old ###
pima.train$Diagnosis <- as.factor(pima.train$Diagnosis)
library(randomForest)
pima.rf_old = randomForest(Diagnosis ~., data = pima.train, ntree = 10, mtry=4)
pima.test$Diagnosis <- as.factor(pima.test$Diagnosis)
pima.rf_old_pred <- predict(pima.rf_old, pima.test, type="prob")[,2]


### new ###
source("randomForestM.R")
pima.rf <- randomForestM(pima.train, 10)
pima.rf_pred <- rf_predict(pima.rf, pima.test)


### plot ###
source("ROC.R")
pdf(file="test1.pdf")


for (i in 1:length(pima.rf_pred)) {
	if (pima.rf_pred[i]==0) { pima.rf_pred[i] <- pima.rf_pred[i]+0.01 }
	else if (pima.rf_pred[i]==1) { pima.rf_pred[i] <- pima.rf_pred[i]-0.01 }
}


plot.roc(pima.rf_old_pred, pima.test$Diagnosis, col = "blue")
lines.roc(pima.rf_pred, pima.test$Diagnosis, col = "red")
leg.txt <- c("Original RF, M=4","My RF, M=4")
legend("bottomright", leg.txt, col=c("blue","red"), lty=c(1, 1), cex=0.8, inset=0.02)


dev.off()










