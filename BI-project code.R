# BI project
#讀取檔案
whiteWine<-read.csv("/Users/melody/Downloads/winequality-white.csv",header = T,sep=";")
names(whiteWine)
str(whiteWine)

#categorical data 把品質分成三等級
whiteWine$quality.level <- factor(ifelse(whiteWine$quality < 5, 0, 
                                          ifelse(whiteWine$quality > 6, 2, 1)))
str(whiteWine)
summary(whiteWine)
dim(whiteWine) #dimension 看有幾個屬性跟data
head(whiteWine)

install.packages("corrplot")
library(ggplot2)
par(mar = rep(2, 4))
pairs(whiteWine)
wineCor<- cor(whiteWine)
library(corrplot)
corrplot(wineCor,tl.srt=45)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
pairs(whiteWine[,1:12], upper.panel = panel.cor,
      diag.panel = panel.hist,
      lower.panel = panel.smooth)


#data split
install.packages("caret")
install.packages("klaR")
install.packages('e1071', dependencies=TRUE)
library(caret)
library(klaR)

summary(whiteWine)
set.seed(1000)
wine_sampling_vector <- createDataPartition(whiteWine$quality.level, p =
                                              0.80, list = FALSE)
whiteWine <- whiteWine[,-12]
wine_train <- whiteWine[wine_sampling_vector,]#training data
wine_test <- whiteWine[-wine_sampling_vector,] #testing data

str(wine_train)
library(e1071)
NULL.MODEL <- nullModel(x=NULL, y=whiteWine$quality.level, newdata=white_full)
confusionMatrix(NULL.MODEL)
names(NULL.MODEL)
print(NULL.MODEL$pct)
x <- wine_train[,1:11]
fit.lda <- train(quality.level ~ alcohol + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide +
                   total.sulfur.dioxide + density + pH + sulphates + fixed.acidity, data=wine_train, method="lda", preProc=c("center", "scale"))
confusionMatrix(fit.lda)

fit.svmRadial <- train(quality.level ~ alcohol + volatile.acidity + citric.acid + residual.sugar + chlorides + 
                         free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + fixed.acidity, 
                         data=wine_train, method="svmRadial", preProc=c("center", "scale"), fit=FALSE)
confusionMatrix(fit.svmRadial)

####
fit.c50 <- train(quality.level ~ alcohol + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide +
                   total.sulfur.dioxide + density + pH + sulphates + fixed.acidity, data=wine_train, method="C5.0")
##### naive bayes
fit.nb <- train(quality.level ~ ., data=wine_train, method="nb")
confusionMatrix(fit.nb)

#random forest
fit.rf <- train(quality.level ~ alcohol + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide +
                  total.sulfur.dioxide + density + pH + sulphates + fixed.acidity, data=wine_train, method="rf")
confusionMatrix(fit.rf)
inTrain <- createDataPartition(whiteWine$quality.level, p =
                                              0.80, list = FALSE)
wine_train <- whiteWine[inTrain,]
wine_test <- whiteWine[-inTrain,]
svm.predict <- predict(fit.svmRadial, wine_test)
confusionMatrix(svm.predict, wine_test$quality.level)

nb.predict <- predict(fit.nb,wine_test)
confusionMatrix(nb.predict, wine_test$quality.level)
#compare model
results <- resamples(list(svm=fit.svmRadial,nb=fit.nb, rf=fit.rf))

results.diff <- diff(results)

results.diff

summary(results.diff)

# Table comparison
summary(results)

# boxplot comparison
bwplot(results)

# Dot-plot comparison
dotplot(results)


rf.predict <- predict(fit.rf, wine_test)
confusionMatrix(rf.predict, wine_test$quality.level)

#另一種方法

# load the libraries
library(caret)
library(klaR)
# load the iris dataset

# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(whiteWine$quality, p=split, list=FALSE)
data_train <- whiteWine[ trainIndex,]
data_test <- whiteWine[-trainIndex,]
str(whiteWine)
# train a naive bayes model
model <- NaiveBayes(quality.level ~ ., data=data_train)
# make predictions
x_test <- data_test[,1:11]
y_test <- data_test[,12]
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions$class, y_test)


# load the library
library(caret)
# load the iris dataset
data(iris)
# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
model <- train(quality.level ~ alcohol + volatile.acidity + citric.acid + residual.sugar + chlorides + 
                 free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + fixed.acidity, data=data_train, trControl=train_control, method="nb", tuneGrid=grid)

#tree

train.rpart <- rpart(quality_fac~.,data=wine_train)
train.rpart
rpart.plot(train.rpart)

predict.rpart <- predict(train.rpart, wine_data)
summary(predict.rpart)
cf1 <- confusionMatrix(predict.rpart, wine_data$quality)
# summarize results
print(model)

ctrl<- trainControl(method = "cv",
                            number=10,
                            savePredictions = T,
                            classProbs = T)
fit.rf <- train(quality.level ~ alcohol + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide +
                  total.sulfur.dioxide + density + pH + sulphates + fixed.acidity, data=wine_train, method="rf", trControl=ctrl)

#PCA analysis
pcaData <- read.csv("/Users/melody/Downloads/winequality-white.csv",header = T,sep=";")
pca = prcomp(formula=~., data= pcaData[,-12],scale=T)
pca
plot(pca, type="line", main= "try")
vars <- (pca$sdev)^2
vars

props <- vars / sum(vars)
props
cumulative.rpops <- cumsum(props)
cumulative.rpops

#補充
##PCA 主成份分析

pcaData <- whiteWine
pca = prcomp(formula=~., data= pcaData[,-12],scale=T)#prcomp為主要函式
pca
plot(pca, type="line", main= "PCA")+abline(h=1, col="blue")#繪製陡坡圖(screet plot)
props <- vars / sum(vars)
props
cumulative.rpops <- cumsum(props)
cumulative.rpops
cumulative.rpops[4]
plot(cumulative.rpops)

#根據凱莎原則，特徵值大於1的主成份就可以選取；而且第四個以後的主成份變異趨於平緩，因此選擇前四個主成份是比較好的選擇。

par(mfrow = c(4,3))
for (i in c(1:11)) {
  plot(whiteWine[, i], jitter(whiteWine[, "quality"]), xlab = names(whiteWine)[i],
       ylab = "quality" , col = "firebrick", cex = 0.8, cex.lab = 2)
  abline(lm(whiteWine[, "quality"] ~ whiteWine[ ,i]), lty = 2, lwd = 2)
}
par(mfrow = c(1, 1))
