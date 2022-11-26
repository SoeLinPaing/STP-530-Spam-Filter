rm(list=ls()) # Clean up the workspace for the new analysis
options(max.print = 100000)

library(dplyr)
library(car)
library(caret)
library(binomTools)

setwd("C:/Users/slinp/Desktop/spambase")

spamdata <- read.csv("spambase.data", header = F)

attach(spamdata)

#splitting the data into train set and test set
train_size = floor(0.8 * nrow(spamdata))
set.seed(102)

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


summary(model)

#################################################################################
#Multicollinearity check
linear_model <- lm(V1 ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 
             + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 
             + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 
             + V56 + V57 , data = Train)
vif(linear_model)

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
subset(melt(cmatrix),value>.60 & value <1.0)
#V31 V30, V32 V30, V34  V30, V36  V30, V32  V31, V34  V31, V34  V32, V40  V32, V36  V34, V40  V34, V32  V36
#V31, 32, 34, 36, 40
#'are the pairs with more than 0.7 correleration coefficient
train1 <- Train[,30:40]
train1 <- train1[,-c(33,35,37,38,39)]
pairs(train1)
#Also check the negative correlation to be safe
subset(melt(cmatrix),value< -.80 & value > -1.0)
#There is no high negative correlation between predictors


############################
#Plot influential points
influencePlot(model)

############################
#Find the influential points using DFBETA
dfbeta_score <- dfbeta(model)
dfbeta_score
subset(melt(dfbeta_score),value > 2)
subset(melt(dfbeta_score),value < -2)




############################
#residual plot
residualPlot(model)


###############################################################################
#Reduce model manually
#Two criterions are considered
#1.Based on the full model, we will get rid of the predictors which pvalue is greater than 0.25

manual.large.model <- glm(V58 ~  V2 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V12 + V14 + V15 + V16 + V17 + V19 
             + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V33 + V35 + V36 + 
             + V38 + V39 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V52 + V53 + V54 + V56 + V57 , data = Train, family = 'binomial')
#43 predictors

summary(manual.large.model)

manual.small.model <- glm(V58 ~  V2 + V5 + V6 + V7 + V8 + V9 + V15 + V16 + V17 + V19 
                          + V20 + V21 + V23 + V24 + V25 + V26 + V27 + V28 + V33 + V35 + V36 + 
                             V39 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V52 + V53 + V56 + V57 , data = Train, family = 'binomial')

summary(manual.small.model)

library(lmtest)
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
#42 predictors
#V2+V4+V5+V6+V7+V8+V9+V12+V15+V16+V17+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29
#+V30+V33+V35+V36+V38+V39+V41+V42+V43+V44+V45+V46+V47+V48+V49+V52+V53+V54+V56+V57+

################################################################################
#Compare manual model vs AIC using confusion matrix because they are not nested models

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

##########################
#manually reduced model
manual.large.model <- train(V58 ~  V2 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V12 + V14 + V15 + V16 + V17 + V19 
                          + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V30 + V33 + V35 + 
                            + V38 + V39 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V52 + V53 + V54 + V56 + V57 , data = Train, method = "glm", family = 'binomial', trControl = ctrl)

pred <- manual.large.model$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)
eachfold <- pred %>%                                        
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
eachfold

########################
#manually reduced model - 2
manual.reduced.model2 <- train(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 
                  + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29  + V33  + V35 + V37 
                  + V38 + V39 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 
                  + V56 + V57, data = Train, method = "glm", family = "binomial", trControl = ctrl)

pred <- manual.reduced.model2$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)
eachfold <- pred %>%  
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
eachfold


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

################################################################################
#Testing model performance on test set

#Testing full model
mod.prob = predict(full_mod, Test, type ="prob")
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
pred = rep("0", nrow(Test))
pred[mod.prob$"1">0.5] = 1

tab = confusionMatrix(data = factor(pred),reference = factor(Test$V58), positive = "1")
tab

####################
#Testing manually reduced model
mod.prob = predict(manual.large.model, Test, type ="prob")
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
pred = rep("0", nrow(Test))
pred[mod.prob$"1">0.5] = 1

tab = confusionMatrix(data = factor(pred),reference = factor(Test$V58), positive = "1")
tab

####################
#Testing manually reduced model-2
mod.prob = predict(manual.reduced.model2, Test, type ="prob")
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
pred = rep("0", nrow(Test))
pred[mod.prob$"1">0.5] = 1

tab = confusionMatrix(data = factor(pred),reference = factor(Test$V58), positive = "1")
tab

####################
#Testing reduced model
mod.prob = predict(reduced_mod, Test, type ="prob")
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
pred = rep("0", nrow(Test))
pred[mod.prob$"1">0.5] = 1

tab = confusionMatrix(data = factor(pred),reference = factor(Test$V58), positive = "1")
tab

####################
#Testing interaction model
mod.prob = predict(interaction_mod, Test, type ="prob")
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
pred = rep("0", nrow(Test))
pred[mod.prob$"1">0.5] = 1

tab = confusionMatrix(data = factor(pred),reference = factor(Test$V58), positive = "1")
tab


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


######################
#Random forest
library('randomForest')
RF.model = randomForest(factor(Train$V58)~.,new_train, ntree = 50, mtry = 19, importance= TRUE)

mod.prob = predict(RF.model, Test)
# Marking the cases where probability is greater that 50% as "yes" for spam and marking
tab_RF = confusionMatrix(mod.prob,actual_outcome)
tab_RF
