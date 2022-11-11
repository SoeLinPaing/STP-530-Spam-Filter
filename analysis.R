rm(list=ls()) # Clean up the workspace for the new analysis

library(dplyr)
library(car)

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
model <- glm(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 
                 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 
                 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 
                 + V56 + V57 , data = Train, family = 'binomial')

#V55 multicollinearity

summary(model)

#Performance on train set
Train$model_prob <- predict(model, Train, type = "response")

Train <- Train %>% 
  mutate(model_pred = 1*(model_prob > .50) + 0)

Train <- Train %>% 
  mutate(accurate = 1*(model_pred == V58))

sum(Train$accurate)/nrow(Train)

#Performance model on test set
Test$model_prob <- predict(model, Test, type = "response")
str(Test$model_prob)

Test <- Test %>% 
  mutate(model_pred = 1*(model_prob > .50) + 0)

Test <- Test %>% 
  mutate(accurate = 1*(model_pred == V58))

sum(Test$accurate)/nrow(Test)

plot(V2, V58)
hist(V2)
pairs(V1:V5)
cor(spamdata)
vif(model)
