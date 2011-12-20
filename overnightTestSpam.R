
setwd("C:/Users/Alex/Documents/SYS 6018/PROJECT")
source("ROC.R")
library(randomForest)
source("randomForestM.R")


spam.full <- read.table("Spam.txt", sep="	", header=TRUE)
spam.full$X1 <- as.factor(spam.full$X1)
i <- sample(1:nrow(spam.full), round(0.33*nrow(spam.full)))
j <- setdiff(1:nrow(spam.full), i)
spam.test <- spam.full[i,]
spam.train <- spam.full[j,]


spam.rf = randomForest(X1 ~., data = spam.train, ntree = 200)

jpeg(file="spam_50trees.jpg")
plot(spam.rf, main = "Spam Random Forest")
dev.off()

# 5 vs 50 variables out of 58. 5 is definitely better
jpeg(file="spam_m5and50.jpg")
spam.rf = randomForest(X1 ~., data = spam.train, ntree = 50, mtry=5)
spam.rf_pred <- predict(spam.rf, spam.test, type="prob")[,2]
plot.roc(spam.rf_pred, spam.test$X1, col = "red")
spam.rf = randomForest(X1 ~., data = spam.train, ntree = 50, mtry=50)
spam.rf_pred <- predict(spam.rf, spam.test, type="prob")[,2]
lines.roc(spam.rf_pred, spam.test$X1, col = "blue")
leg.txt <- c("RF m=5","RF m=50")
legend("bottomright", leg.txt, col=c("red","blue"), lty=c(1, 1), cex=0.8, inset=0.02)
dev.off()


# 5 vs 15 variables out of 58. 5 is definitely better
jpeg(file="spam_m5and15.jpg")
spam.rf = randomForest(X1 ~., data = spam.train, ntree = 50, mtry=5)
spam.rf_pred <- predict(spam.rf, spam.test, type="prob")[,2]
plot.roc(spam.rf_pred, spam.test$X1, col = "red")
spam.rf = randomForest(X1 ~., data = spam.train, ntree = 50, mtry=15)
spam.rf_pred <- predict(spam.rf, spam.test, type="prob")[,2]
lines.roc(spam.rf_pred, spam.test$X1, col = "blue")
leg.txt <- c("RF m=5","RF m=15")
legend("bottomright", leg.txt, col=c("red","blue"), lty=c(1, 1), cex=0.8, inset=0.02)
dev.off()


# 5 vs 10 variables out of 58. 10 is better
jpeg(file="spam_m5and10.jpg")
spam.rf = randomForest(X1 ~., data = spam.train, ntree = 50, mtry=5)
spam.rf_pred <- predict(spam.rf, spam.test, type="prob")[,2]
plot.roc(spam.rf_pred, spam.test$X1, col = "red")
spam.rf = randomForest(X1 ~., data = spam.train, ntree = 50, mtry=10)
spam.rf_pred <- predict(spam.rf, spam.test, type="prob")[,2]
lines.roc(spam.rf_pred, spam.test$X1, col = "blue")
leg.txt <- c("RF m=5","RF m=10")
legend("bottomright", leg.txt, col=c("red","blue"), lty=c(1, 1), cex=0.8, inset=0.02)
dev.off()


jpeg(file="spam_m10trees50.jpg")
spam.rf = randomForest(X1 ~., data = spam.train, ntree = 50, mtry=10)
spam.rf_pred <- predict(spam.rf, spam.test, type="prob")[,2]
plot.roc(spam.rf_pred, spam.test$X1, col = "blue")
leg.txt <- c("Original RF 50 trees, m=5")
legend("bottomright", leg.txt, col=c("red"), lty=c(1), cex=0.8, inset=0.02)
dev.off()








###############
### in the hwk i determined 30 trees and m=5 so i'll run with that first
###############

### old method ###
pima.rf_old = randomForest(Diagnosis ~., data = pima.train, ntree = 30, mtry=5)
pima.test$Diagnosis <- as.factor(pima.test$Diagnosis)
pima.rf_old_pred <- predict(pima.rf_old, pima.test, type="prob")[,2]


### my baseline method ###
pima.rf <- randomForestM(pima.train, 30, -5)
pima.rf_pred <- rf_predict(pima.rf, pima.test)


#pdf(file="test_baseline.pdf")

#plot.roc(pima.rf_pred, pima.test$Diagnosis, col = "red")
#lines.roc(pima.rf_old_pred, pima.test$Diagnosis, col = "blue")
#leg.txt <- c("Original RF, M=5","My RF, M=5")
#legend("bottomright", leg.txt, col=c("blue","red"), lty=c(1, 1), cex=0.8, inset=0.02)

#dev.off()


###################################################################


### method 1 ###
#pima.rf1 <- randomForestM(pima.train, 30, 1)
#pima.rf_pred1 <- rf_predict(pima.rf1, pima.test)


#pdf(file="test_method1.pdf")

#plot.roc(pima.rf_pred, pima.test$Diagnosis, col = "red")
#lines.roc(pima.rf_old_pred, pima.test$Diagnosis, col = "blue")
#lines.roc(pima.rf_pred1, pima.test$Diagnosis, col = "green")
#leg.txt <- c("Original RF, M=5","My RF, M=5", "METHOD 1")
#legend("bottomright", leg.txt, col=c("blue","red","green"), lty=c(1, 1, 1), cex=0.8, inset=0.02)

#dev.off()


###################################################################


### method 2 ###
pima.rf2 <- randomForestM(pima.train, 30, 2)
pima.rf_pred2 <- rf_predict(pima.rf2, pima.test)


pdf(file="test_method2.pdf")

plot.roc(pima.rf_pred, pima.test$Diagnosis, col = "red")
lines.roc(pima.rf_old_pred, pima.test$Diagnosis, col = "blue")
lines.roc(pima.rf_pred2, pima.test$Diagnosis, col = "green")
leg.txt <- c("Original RF, M=5","My RF, M=5", "METHOD 2")
legend("bottomright", leg.txt, col=c("blue","red","green"), lty=c(1, 1, 1), cex=0.8, inset=0.02)

dev.off()


###################################################################


### method 3 ###
pima.rf3 <- randomForestM(pima.train, 30, 3)
pima.rf_pred3 <- rf_predict(pima.rf3, pima.test)


pdf(file="test_method3.pdf")

plot.roc(pima.rf_pred, pima.test$Diagnosis, col = "red")
lines.roc(pima.rf_old_pred, pima.test$Diagnosis, col = "blue")
lines.roc(pima.rf_pred3, pima.test$Diagnosis, col = "green")
leg.txt <- c("Original RF, M=5","My RF, M=5", "METHOD 3")
legend("bottomright", leg.txt, col=c("blue","red","green"), lty=c(1, 1, 1), cex=0.8, inset=0.02)

dev.off()


###################################################################


### method 4 ###
pima.rf4 <- randomForestM(pima.train, 30, 4)
pima.rf_pred4 <- rf_predict(pima.rf4, pima.test)


pdf(file="test_method4.pdf")

plot.roc(pima.rf_pred, pima.test$Diagnosis, col = "red")
lines.roc(pima.rf_old_pred, pima.test$Diagnosis, col = "blue")
lines.roc(pima.rf_pred4, pima.test$Diagnosis, col = "green")
leg.txt <- c("Original RF, M=5","My RF, M=5", "METHOD 4")
legend("bottomright", leg.txt, col=c("blue","red","green"), lty=c(1, 1, 1), cex=0.8, inset=0.02)

dev.off()


###################################################################


### method 5 ###
pima.rf5 <- randomForestM(pima.train, 30, 5)
pima.rf_pred5 <- rf_predict(pima.rf5, pima.test)


pdf(file="test_method5.pdf")

plot.roc(pima.rf_pred, pima.test$Diagnosis, col = "red")
lines.roc(pima.rf_old_pred, pima.test$Diagnosis, col = "blue")
lines.roc(pima.rf_pred5, pima.test$Diagnosis, col = "green")
leg.txt <- c("Original RF, M=5","My RF, M=5", "METHOD 5")
legend("bottomright", leg.txt, col=c("blue","red","green"), lty=c(1, 1, 1), cex=0.8, inset=0.02)

dev.off()


###################################################################


### method 6 ###
pima.rf6 <- randomForestM(pima.train, 30, 6)
pima.rf_pred6 <- rf_predict(pima.rf6, pima.test)


pdf(file="test_method6.pdf")

plot.roc(pima.rf_pred, pima.test$Diagnosis, col = "red")
lines.roc(pima.rf_old_pred, pima.test$Diagnosis, col = "blue")
lines.roc(pima.rf_pred6, pima.test$Diagnosis, col = "green")
leg.txt <- c("Original RF, M=5","My RF, M=5", "METHOD 6")
legend("bottomright", leg.txt, col=c("blue","red","green"), lty=c(1, 1, 1), cex=0.8, inset=0.02)

dev.off()











