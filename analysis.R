rm(list=ls()) # Clean up the workspace for the new analysis
options(max.print = 100000)
par(mfrow = c(1,1))

library(dplyr)
library(car)
library(caret)
library(binomTools)
library(yardstick)
library("DescTools")
library(lmtest)


setwd("C:/Users/slinp/Desktop/spambase")

spamdata <- read.csv("spambase.data", header = F)

attach(spamdata)

percentage_combined <- rowSums(spamdata[, 1:54])
h <- hist(percentage_combined)
plot(h, xlab = "combined percentage of words in each observation", ylab = "number of observations",
     main = "Histogram of combined percentage of words in each observation")
#splitting the data into train set and test set
train_size = floor(0.8 * nrow(spamdata)) #80% of the data are in the train set
set.seed(102) #set the seed to get the same sequence of random shuffling everytime

train_ind = sample(seq_len(nrow(spamdata)), size = train_size)
Train = spamdata[train_ind, ]
Test = spamdata[-train_ind, ]
str(train_ind)
sort(train_ind, decreasing = FALSE, na.last = NA)


#Fitting the full model
################################################################################
model <- glm(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 
                 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 
                 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 
                 + V56 + V57 , data = Train, family = 'binomial')

PseudoR2(model)
summary(model)
AIC(model)




#################################################################################
#Multicollinearity check
linear_model <- lm(V58 ~V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 
             + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 
             + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 
             + V56 + V57 , data = Train)

vif(linear_model)
#vif values are significantly shrunk for logistic regression

#linear model is necessary is necessary to detect the large vif
linear_model1 <- lm(V1 ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 
                   + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 
                   + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 
                   + V56 + V57 , data = Train, family = 'binomial')
vif(linear_model1)
#v32 and v34 has over 100 vif, remove them

#############################
#Using correlation coefficient, find predictors with correlation higher than 0.5
cmatrix <- cor(spamdata)
cmatrix
library(reshape2)

#find the predictor pairs with more than 0.6 correlation
subset(melt(cmatrix),value>.60 & value <1.0)

#plot the pairs with more than 0.6 correlation
train1 <- Train[,30:40]
train2 <- train1[,-c(4,6,8,9,10)]
pairs(train2)

#Also check the negative correlation to be safe
subset(melt(cmatrix),value< -.80 & value > -1.0)
#There is no negative correlation between predictors



###############################################################################
#Plot influential points
influencePlot(model)


############################
#Find the influential points using DFBETA
dfbeta_score <- dfbeta(model)
dfbeta_score

#find the points with dfbeta_score of more than 2 and less than -2
subset(melt(dfbeta_score),value > 2)
subset(melt(dfbeta_score),value < -2)
influencePlot(model)



###############################################################################
#residual plot
residualPlot(model)


###############################################################################
#Reduce model manually -1
#remove predictors with multicollinearity

manual.model <- glm(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 
                   + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29  + V33 + V35 + V37 
                   + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 
                   + V56 + V57 , data = Train, family = 'binomial')
#43 predictors
PseudoR2(manual.model)
summary(manual.model)

barplot(manual.model$coefficients,
        horiz = TRUE)
###############################################################################
#Reduce model manually -2
#Two criterions are considered
#1.Based on the full model, we will get rid of the predictors which pvalue is greater than 0.25
#2. Based on the resulted model, we will get rid of the predictors with pvalue greater than 0.1
#Use likelihood ration test to decide which model is better

#1.
manual.large.model <- glm(V58 ~  V2 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V12 + V14 + V15 + V16 + V17 + V19 
             + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29  + V33 + V35 + V36 + 
             + V38 + V39 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V52 + V53 + V54 + V56 + V57 , data = Train, family = 'binomial')
#43 predictors
PseudoR2(manual.large.model)
summary(manual.large.model)


#2.
manual.small.model <- glm(V58 ~  V2 + V5 + V6 + V7 + V8 + V9 + V15 + V16 + V17 + V19 
                          + V20 + V21 + V23 + V24 + V25 + V26 + V27 + V28 + V33 + V35 + V36 + 
                             V39 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V52 + V53 + V56 + V57 , data = Train, family = 'binomial')

summary(manual.small.model)


lrtest(manual.large.model, manual.small.model)
#null: use reduced model
#alternative: use full model
#according to likelihood ratio test, p value is very small close to 0. Thus, we reject null hypothesis and use full model.

#2. Based on the multicolinearity test, the 32 and 34 should be rejected. However, they were already rejected based on p value criterion.
#10, 14 are not in the AIC model. 41 is not in manually reduced model. (mail, report,cs)




################################################################################
# Reduced model using Automated forward selection with AIC
min.mod <- glm(V58 ~ 1, data=Train, family=binomial)
max.mod <- formula(glm(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 
                       + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 
                       + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 
                       + V56 + V57 , data = Train, family = 'binomial'))
#max.mod <- formula(glm(Class ~ .*., data=train, family=binomial))
fwd.mod <- step(min.mod, direction='forward', scope=max.mod, saveprediction= TR)
fwd.mod$anova$AIC
summary(fwd.mod)

#The final reduced model resulted from AIC
reduced.mod <- glm(formula = V58 ~ V53 + V7 + V25 + V27 + V56 + V16 + V46 + 
        V42 + V17 + V23 + V21 + V52 + V45 + V5 + V24 + V48 + V41 + 
        V57 + V8 + V44 + V20 + V33 + V6 + V29 + V43 + V4 + V36 + 
        V39 + V54 + V49 + V47 + V19 + V35 + V9 + V28 + V26 + V2 + 
        V15 + V38 + V30 + V22 + V12, family = binomial, data = Train)
PseudoR2(reduced.mod)
summary(reduced.mod)
AIC(reduced.mod)
#42 predictors
#V2+V4+V5+V6+V7+V8+V9+V12+V15+V16+V17+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29
#+V30+V33+V35+V36+V38+V39+V41+V42+V43+V44+V45+V46+V47+V48+V49+V52+V53+V54+V56+V57+


################################################################################
#interaction model. initial model for it is from the reduced model 
init_mod <- glm(V58 ~ V53 + V7 + V25 + V27 + V56 + V16 + V46 + 
                 V42 + V17 + V23 + V21 + V52 + V45 + V5 + V24 + V48 + V41 + 
                 V57 + V8 + V44 + V20 + V33 + V6 + V29 + V43 + V4 + V36 + 
                 V39 + V54 + V49 + V47 + V19 + V35 + V9 + V28 + V26 + V2 + 
                 V15 + V38 + V30 + V22 + V12, family = binomial, data = Train)
fwd.intermod <- step(init_mod, scope = . ~ .^2, direction = 'forward', saveprediction= TR)
fwd.intermod$anova$AIC
summary(fwd.intermod)


#the final interaction model from AIC is 
inter.mod <- glm(formula = V58 ~ V53 + V7 + V25 + V27 + V56 + V16 + V46 + 
     V42 + V17 + V23 + V21 + V52 + V45 + V5 + V24 + V48 + V41 + 
     V57 + V8 + V44 + V20 + V33 + V6 + V29 + V43 + V4 + V36 + 
     V39 + V54 + V49 + V47 + V19 + V35 + V9 + V28 + V26 + V2 + 
     V15 + V38 + V30 + V22 + V12 + V52:V57 + V25:V36 + V24:V57 + 
     V16:V36 + V27:V5 + V7:V8 + V46:V28 + V21:V28 + V17:V6 + V52:V8 + 
     V35:V2 + V16:V19 + V36:V28 + V17:V9 + V7:V17 + V19:V12 + 
     V56:V5 + V25:V12 + V46:V24 + V43:V12 + V16:V26 + V7:V23 + 
     V42:V17 + V56:V6 + V46:V57 + V52:V45 + V28:V12 + V49:V22 + 
     V5:V24 + V17:V52 + V56:V16 + V16:V45 + V52:V49 + V21:V19 + 
     V46:V22 + V7:V54 + V27:V9 + V45:V12 + V46:V12 + V54:V12 + 
     V45:V57 + V46:V26 + V23:V6 + V53:V15 + V47:V38 + V25:V19 + 
     V56:V46 + V25:V44 + V45:V20 + V8:V19 + V27:V21 + V56:V24 + 
     V17:V54 + V44:V35 + V56:V28, family = binomial, data = Train)


summary(inter.mod)

################################################################################
#Cross Validation using k-fold method (number of folds = 5)
set.seed(2011)
ctrl <- trainControl(method = "cv", number = 5, savePredictions=TRUE)
Train$V58 = as.factor(Train$V58)
#full model
full_mod <- train(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 
                  + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 
                  + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 
                  + V56 + V57, data = Train, method = "glm", family = "binomial", trControl = ctrl)

pred <- full_mod$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)
eachfold <- pred %>%  
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
eachfold

sum(eachfold$Accuracy)/5
########################
#manually reduced model - 1
manual.reduced.model1 <- train(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 
                  + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29  + V33 + V35 + V37 
                  + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 
                  + V56 + V57, data = Train, method = "glm", family = "binomial", trControl = ctrl)

pred <- manual.reduced.model1$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)
eachfold <- pred %>%  
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
eachfold

sum(eachfold$Accuracy)/5

##########################
#manually reduced model -2
manual.large.model <- train(V58 ~  V2 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V12 + V14 + V15 + V16 + V17 + V19 
                            + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V33 + V35 + 
                              + V38 + V39 + V40 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V52 + V53 + V54 + V56 + V57 , data = Train, method = "glm", family = 'binomial', trControl = ctrl)

pred <- manual.large.model$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)
eachfold <- pred %>%                                        
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
eachfold

sum(eachfold$Accuracy)/5

########################
#reduced model
reduced_mod <- train(V58 ~ V53 + V7 + V25 + V27 + V56 + V16 + V46 + 
                      V42 + V17 + V23 + V21 + V52 + V45 + V5 + V24 + V48 + V41 + 
                      V57 + V8 + V44 + V20 + V33 + V6 + V29 + V43 + V4 + V36 + 
                      V39 + V54 + V49 + V47 + V19 + V35 + V9 + V28 + V26 + V2 + 
                       V15 + V38 + V30 + V22 + V12, data = Train, method = "glm", family = "binomial", trControl = ctrl)


pred <- reduced_mod$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)
eachfold <- pred %>%                                        
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
eachfold
sum(eachfold$Accuracy)/5

###########################
#interaction model
interaction_mod <- train(V58 ~ V53 + V7 + V25 + V27 + V56 + V16 + V46 + 
                           V42 + V17 + V23 + V21 + V52 + V45 + V5 + V24 + V48 + V41 + 
                           V57 + V8 + V44 + V20 + V33 + V6 + V29 + V43 + V4 + V36 + 
                           V39 + V54 + V49 + V47 + V19 + V35 + V9 + V28 + V26 + V2 + 
                           V15 + V38 + V30 + V22 + V12 + V52:V57 + V25:V36 + V24:V57 + 
                           V16:V36 + V27:V5 + V7:V8 + V46:V28 + V21:V28 + V17:V6 + V52:V8 + 
                           V35:V2 + V16:V19 + V36:V28 + V17:V9 + V7:V17 + V19:V12 + 
                           V56:V5 + V25:V12 + V46:V24 + V43:V12 + V16:V26 + V7:V23 + 
                           V42:V17 + V56:V6 + V46:V57 + V52:V45 + V28:V12 + V49:V22 + 
                           V5:V24 + V17:V52 + V56:V16 + V16:V45 + V52:V49 + V21:V19 + 
                           V46:V22 + V7:V54 + V27:V9 + V45:V12 + V46:V12 + V54:V12 + 
                           V45:V57 + V46:V26 + V23:V6 + V53:V15 + V47:V38 + V25:V19 + 
                           V56:V46 + V25:V44 + V45:V20 + V8:V19 + V27:V21 + V56:V24 + 
                           V17:V54 + V44:V35 + V56:V28 , data = Train, method = "glm", family = "binomial", trControl = ctrl)

pred <- interaction_mod$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)
eachfold <- pred %>%                                        
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
eachfold
sum(eachfold$Accuracy)/5

################################################################################
#Testing model performance on test set

#Testing full model
mod.prob = predict(full_mod, Test, type ="prob")
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
pred = rep("0", nrow(Test))
pred[mod.prob$"1">0.5] = 1

tab = confusionMatrix(data = factor(pred),reference = factor(Test$V58), positive = "1")
tab

#Plot confusion matrix
truth_predicted <- data.frame(Test$V58, pred)
truth_predicted$Test.V58 <- as.factor(truth_predicted$Test.V58)
truth_predicted$pred <- as.factor(truth_predicted$pred)
cm <- conf_mat(truth_predicted, Test.V58, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+ggtitle("Full model")+
  theme(plot.title = element_text(hjust = 0.5))
####################
#Testing manually reduced model -2
mod.prob = predict(manual.large.model, Test, type ="prob")
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
pred = rep("0", nrow(Test))
pred[mod.prob$"1">0.5] = 1

tab = confusionMatrix(data = factor(pred),reference = factor(Test$V58), positive = "1")
tab

#Plot confusion matrix
truth_predicted <- data.frame(Test$V58, pred)
truth_predicted$Test.V58 <- as.factor(truth_predicted$Test.V58)
truth_predicted$pred <- as.factor(truth_predicted$pred)
cm <- conf_mat(truth_predicted, Test.V58, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+ggtitle("Reduced model 2")+
  theme(plot.title = element_text(hjust = 0.5))
####################
#Testing manually reduced model-1
mod.prob = predict(manual.reduced.model1, Test, type ="prob")
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
pred = rep("0", nrow(Test))
pred[mod.prob$"1">0.5] = 1

tab = confusionMatrix(data = factor(pred),reference = factor(Test$V58), positive = "1")
tab

#Plot confusion matrix
truth_predicted <- data.frame(Test$V58, pred)
truth_predicted$Test.V58 <- as.factor(truth_predicted$Test.V58)
truth_predicted$pred <- as.factor(truth_predicted$pred)
cm <- conf_mat(truth_predicted, Test.V58, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+ggtitle("Reduced model 1")+
  theme(plot.title = element_text(hjust = 0.5))

####################
#Testing reduced model
mod.prob = predict(reduced_mod, Test, type ="prob")
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
pred = rep("0", nrow(Test))
pred[mod.prob$"1">0.5] = 1

tab = confusionMatrix(data = factor(pred),reference = factor(Test$V58), positive = "1")
tab

#Plot confusion matrix
truth_predicted <- data.frame(Test$V58, pred)
truth_predicted$Test.V58 <- as.factor(truth_predicted$Test.V58)
truth_predicted$pred <- as.factor(truth_predicted$pred)
cm <- conf_mat(truth_predicted, Test.V58, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+ggtitle("AIC Reduced model")+
  theme(plot.title = element_text(hjust = 0.5))


####################
#Testing interaction model
mod.prob = predict(interaction_mod, Test, type ="prob")
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
pred = rep("0", nrow(Test))
pred[mod.prob$"1">0.5] = 1

tab = confusionMatrix(data = factor(pred),reference = factor(Test$V58), positive = "1")
tab

#Plot confusion matrix
truth_predicted <- data.frame(Test$V58, pred)
truth_predicted$Test.V58 <- as.factor(truth_predicted$Test.V58)
truth_predicted$pred <- as.factor(truth_predicted$pred)
cm <- conf_mat(truth_predicted, Test.V58, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+ggtitle("Interaction model")+
  theme(plot.title = element_text(hjust = 0.5))
################################################################################
#Likelihood ratio testings
lrtest(model, manual.model)


##################################################################################
#Normalize 55 56 57 attributes
spamdata.subset<-spamdata[c(-55,-56,-57,-58)]
data = spamdata[c(55,56,57)]
##New normalised dataest is ready
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))*100) }

data.n<- as.data.frame(lapply(data[,1:3], normalize))
spamdata.subset[55]<-data.n[1]
spamdata.subset[56]<-data.n[2]
spamdata.subset[57]<-data.n[3]
spamdata.subset[58]<-spamdata[58]
dataset<-spamdata.subset
attach(dataset)
#splitting the data into train set and test set
train_size = floor(0.8 * nrow(spamdata.subset))
set.seed(102)

train_ind = sample(seq_len(nrow(spamdata.subset)), size = train_size)
Train = spamdata.subset[train_ind, ]
Test = spamdata.subset[-train_ind, ]
str(train_ind)
sort(train_ind, decreasing = FALSE, na.last = NA)

################################################################################
#Machine learning models
#Naive Bayes
new_train <- Train[1:57]
library('naivebayes')
naive_bayes.model <- naive_bayes(factor(Train$V58) ~ ., data = new_train, usekernel = T) 
summary(naive_bayes.model)
plot(naive_bayes.model)

actual_outcome = factor(Test$V58)
mod.prob = predict(naive_bayes.model, Test)
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
tab = confusionMatrix(mod.prob,actual_outcome)
tab
#Plot confusion matrix
pred = rep("0", nrow(Test))
pred = mod.prob
truth_predicted <- data.frame(Test$V58, pred)
truth_predicted$Test.V58 <- as.factor(truth_predicted$Test.V58)
truth_predicted$pred <- as.factor(truth_predicted$pred)
cm <- conf_mat(truth_predicted, Test.V58, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+ggtitle("Naive Bayes")+
  theme(plot.title = element_text(hjust = 0.5))

######################
#Random forest
library('randomForest')
RF.model = randomForest(factor(Train$V58)~.,new_train, ntree = 50, mtry = 19, importance= TRUE)

mod.prob = predict(RF.model, Test)
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
tab_RF = confusionMatrix(mod.prob,actual_outcome)
tab_RF
#Plot confusion matrix
truth_predicted <- data.frame(Test$V58, mod.prob)
truth_predicted$Test.V58 <- as.factor(truth_predicted$Test.V58)
truth_predicted$mod.prob <- as.factor(truth_predicted$mod.prob)
cm <- conf_mat(truth_predicted, Test.V58, mod.prob)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+ggtitle("Random Forest")+
  theme(plot.title = element_text(hjust = 0.5))

####################
#k-nearest neighbour
library('class')
knn.60 <- knn(train=Train, test=Test, cl=Train$V58, k=60)
knn.61 <- knn(train=Train, test=Test, cl=Train$V58, k=61)

confusionMatrix(table(knn.60 ,Test$V58))
confusionMatrix(table(knn.61 ,Test$V58))
#Plot confusion matrix
truth_predicted <- data.frame(Test$V58, knn.60)
truth_predicted$Test.V58 <- as.factor(truth_predicted$Test.V58)
truth_predicted$knn.60 <- as.factor(truth_predicted$knn.60)
cm <- conf_mat(truth_predicted, Test.V58, knn.60)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+ggtitle("KNN-60")+
  theme(plot.title = element_text(hjust = 0.5))

