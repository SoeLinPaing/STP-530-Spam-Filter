rm(list=ls()) # Clean up the workspace for the new analysis
options(max.print = 10000)

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



#######################
colnames(spamdata) <- c('make','address','all','3d','our','over','remove','internet','order','mail',
                        'receive','will','people','report','addresses','free','business','email','you','credit',
                        'your','font','000','money','hp','hpl','george','650','lab','labs',
                        'telnet','857','data','415','85','technology','1999','parts','pm','direct',
                        'cs','meeting','original','project','re','edu','table','conference',';','(',
                        '[','!','$','#','capital_run_length_average','capital_run_length_longest','capital_run_length_total', 'class')

train_ind = sample(seq_len(nrow(spamdata)), size = train_size)
Train = spamdata[train_ind, ]
Test = spamdata[-train_ind, ]
str(train_ind)
sort(train_ind, decreasing = FALSE, na.last = NA)

#######################
attach(spamdata)
spam = spamdata[spamdata$class == 1,]
non.spam = spamdata[spamdata$class == 0,]

#######################
#individual histograms
par(mfrow = c(1,1))
hist(make)
hist(address)
hist(all)
hist(spamdata$"3d")
hist(our)
hist(over)
hist(remove)
hist(internet)
hist(order)
hist(receive)
hist(will)
hist(people)
hist(report)
hist(addresses)
hist(free)
hist(business)
hist(email)
hist(you)
hist(credit)
hist(your)
hist(font)
hist(spamdata$'000')
hist(money)
hist(hp)
hist(hpl)
hist(george)
hist(spamdata$'650')
hist(lab)
hist(labs)
hist(telnet)
hist(spamdata$'857')
hist(data)
hist(spamdata$'415')
hist(spamdata$'85')
hist(technology)
hist(spamdata$'1999')
hist(parts)
hist(pm)
hist(direct)
hist(cs)
hist(meeting)
hist(original)
hist(project)
hist(re)
hist(edu)
hist(table)
hist(conference)
hist(spamdata$";")
hist(spamdata$"(")
hist(spamdata$"[")
hist(spamdata$"!")
hist(spamdata$"$")
hist(spamdata$"#")
hist(capital_run_length_average)
hist(capital_run_length_longest)
hist(capital_run_length_total)

#######################
#spam vs non-spam histograms
par(mfrow=c(1,2))
hist(spam$make)
hist(non.spam$make)

hist(spam$address)
hist(non.spam$address)

hist(spam$all)
hist(non.spam$all)

hist(spam$'3d')
hist(non.spam$'3d')

hist(spam$our)
hist(non.spam$our)

hist(spam$over)
hist(non.spam$over)

hist(spam$remove)
hist(non.spam$remove)

hist(spam$internet)
hist(non.spam$internet)

hist(spam$order)
hist(non.spam$order)

hist(spam$mail)
hist(non.spam$mail)

hist(spam$receive)
hist(non.spam$receive)

hist(spam$will)
hist(non.spam$will)

hist(spam$people)
hist(non.spam$people)

hist(spam$report)
hist(non.spam$report)

hist(spam$addresses)
hist(non.spam$addresses)

hist(spam$free)
hist(non.spam$free)

hist(spam$business)
hist(non.spam$business)

hist(spam$email)
hist(non.spam$email)

hist(spam$you)
hist(non.spam$you)

hist(spam$credit)
hist(non.spam$credit)

hist(spam$your)
hist(non.spam$your)

hist(spam$font)
hist(non.spam$font)

hist(spam$'000')
hist(non.spam$'000')

hist(spam$money)
hist(non.spam$money)

hist(spam$hp)
hist(non.spam$hp)

hist(spam$hpl)
hist(non.spam$hpl)

hist(spam$george)
hist(non.spam$george)

hist(spam$'650')
hist(non.spam$'650')

hist(spam$lab)
hist(non.spam$lab)

hist(spam$labs)
hist(non.spam$labs)

hist(spam$telnet)
hist(non.spam$telnet)

hist(spam$'857')
hist(non.spam$'857')

hist(spam$data)
hist(non.spam$data)

hist(spam$'415')
hist(non.spam$'415')

hist(spam$'85')
hist(non.spam$'85')

hist(spam$technology)
hist(non.spam$technology)

hist(spam$'1999')
hist(non.spam$'1999')

hist(spam$parts)
hist(non.spam$parts)

hist(spam$pm)
hist(non.spam$pm)

hist(spam$direct)
hist(non.spam$direct)

hist(spam$cs)
hist(non.spam$cs)

hist(spam$meeting)
hist(non.spam$meeting)

hist(spam$original)
hist(non.spam$original)

hist(spam$project)
hist(non.spam$project)

hist(spam$re)
hist(non.spam$re)

hist(spam$edu)
hist(non.spam$edu)

hist(spam$table)
hist(non.spam$table)

hist(spam$conference)
hist(non.spam$conference)

hist(spam$';')
hist(non.spam$';')

hist(spam$'(')
hist(non.spam$'(')

hist(spam$'[')
hist(non.spam$'[')

hist(spam$'!')
hist(non.spam$'!')

hist(spam$'$')
hist(non.spam$'$')

hist(spam$'#')
hist(non.spam$'#')

hist(spam$capital_run_length_average)
hist(non.spam$capital_run_length_average)

hist(spam$capital_run_length_longest)
hist(non.spam$capital_run_length_longest)

hist(spam$capital_run_length_total)
hist(non.spam$capital_run_length_total)
#######################
#Why does it make sense to remove 10, 14 and 41 in manually reduced model
par(mfrow = c(1,1))
hist(cs) #V41
hist(mail) #V10
hist(report) #V14

par(mfrow=c(1,2))
hist(spam$mail)
hist(non.spam$mail)

par(mfrow=c(1,2))
hist(spam$report)
hist(non.spam$report)

par(mfrow=c(1,2))
hist(spam$cs)
hist(non.spam$cs)

## proof by contrast
par(mfrow=c(1,2))
hist(spam$"!")
hist(non.spam$"!")
