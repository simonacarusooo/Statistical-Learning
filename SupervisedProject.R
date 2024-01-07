#########################
###SUPERVISED LEARNING###
#########################
#https://www.kaggle.com/datasets/mssmartypants/water-quality

#Data download and data structure
data <- read.csv("C:/Users/39334/Desktop/Stat Learn/waterQuality1.csv", encoding="UTF-8", stringsAsFactors=F)
str(data)
dim(data)
summary(data)
data$ammonia <- as.numeric(data$ammonia)
data$is_safe <- as.factor(data$is_safe)
table(data$is_safe)
data$is_safe <- droplevels(data$is_safe, exclude = "#NUM!")
data <- data[complete.cases(data$is_safe), ]
data <- subset(data, data$ammonia >= 0 )

str(data)
dim(data)


#UNIVARIATE STATISTICS
library(ggplot2)
library(tidyverse)
summary(data)

var_box <-data[,-21] %>% gather(variable,values,1:20)

var_box %>%
  ggplot(aes(x = values)) +                     
  facet_wrap(~ variable, scales = "free") +   
  geom_density(fill = "indianred3")

ggplot(var_box)+
  geom_boxplot(aes(x=variable,y=values), fill="indianred3") + 
  facet_wrap(~variable,ncol=4,scales="free") + 
  theme_minimal()+
  theme(strip.text.x = element_blank(),
        text = element_text(size=8))

#BIVARIATE STATISTICS
library(corrplot)

data1 <- data
data1$is_safe <- as.numeric(data1$is_safe)
data1
matr_cor <- cor(data1)
corrplot(matr_cor, method="number", number.cex=0.65, tl.col="black",tl.cex=0.7)



#########################
###SUPERVISED LEARNING###
#########################
library(caret)
library(pROC)

#Standardization and train-test split
data_st <- data
data1 <- scale(data_st[,-21])
data_st[1:20] <- data1
summary(data_st)

set.seed(100)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

train_data_st <- data_st[train_index, ]
test_data_st <- data_st[-train_index, ]

#Logistic Regression
library(gtsummary)
library(knitr)


logistic <- glm(is_safe ~ ., data = train_data, family = binomial)
summary(logistic)
exp(coef(logistic))
results <- tbl_regression(logistic, exponentiate=F)
results
gt::gtsave(as_gt(results), file = "C:\\Users\\39334\\Desktop\\Stat Learn\\p5.png")

predictions_logistic <- predict(logistic, newdata = test_data, type = "response")
binary_predictions_logistic <- ifelse(predictions_logistic > 0.5, 1, 0)

binary_predictions_logistic <- as.factor(binary_predictions_logistic)
binary_predictions_logistic <- unname(binary_predictions_logistic)
confusion_matrix_logistic <- confusionMatrix(binary_predictions_logistic, test_data$is_safe)
confusion_matrix_logistic
confusion_matrix_logistic$byClass


#Lasso
library(glmnet)
x <- model.matrix(is_safe~.,train_data_st)
y <- as.numeric(train_data_st$is_safe)

set.seed(100)
cv_out <- cv.glmnet(x,y,alpha=1,family="binomial")
plot(cv_out)
lambda_min <- cv_out$lambda.min
lambda_min
coef(cv_out,s=lambda_min)

x_test <- model.matrix(is_safe~., test_data_st)
predictions_lasso <- predict(cv_out, newx = x_test, s=lambda_min, type = "response")
binary_predictions_lasso <- ifelse(predictions_lasso> 0.5, 1, 0)
binary_predictions_lasso <- as.factor(binary_predictions_lasso)
binary_predictions_lasso <- unname(binary_predictions_lasso)
confusion_matrix_lasso <- confusionMatrix(binary_predictions_lasso, test_data_st$is_safe)
confusion_matrix_lasso


#Classification trees
library(rpart)
library(rpart.plot)
library(rattle)
library(tree)

tree <- rpart(is_safe ~ ., data = train_data, method = "class")
rpart.plot(tree, main = "Decision Tree for Water Quality Dataset")
tree

predictions_tree <- predict(tree, newdata = test_data, type = "class")
confusion_matrix_tree <- confusionMatrix(predictions_tree, test_data$is_safe)
confusion_matrix_tree
confusion_matrix_tree$byClass

printcp(tree) 
tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
plotcp(tree)

#Random Forests
library(randomForest)
set.seed(100)
rf = randomForest(is_safe ~ ., data=train_data, ntree=50, importance=TRUE)
varImpPlot(rf)

predictions_rf <- predict(rf, newdata = test_data, type = "class")
confusion_matrix_rf <- confusionMatrix(predictions_rf, test_data$is_safe)
confusion_matrix_rf
confusion_matrix_rf$byClass


