library(openxlsx)
library(dplyr)
library(ggplot2)
library(scales)
library(varhandle)
library(xts)
library(RcppRoll)
library(radiant)
library(ROCR)


#install.packages("radiant.model")
library(radiant.model)

setwd("C:/Users/Natalya/Fraud Analytics")
model.card <- read.csv2("final_df_new_2.csv",header = TRUE,sep = ",")

colnames(model.card)[colSums(is.na(model.card)) > 0]
sapply(model.card,function(x) sum(is.na(x)))

model.card.sel <- model.card[,c(1:4,9:11,16:ncol(model.card))]
colnames(model.card.sel)[colSums(is.na(model.card.sel)) > 0]
model.card.sel[is.na(model.card.sel)] <- 0

str(model.card.sel)

model.card.sel$fraud <- as.factor(model.card.sel$fraud)
model.card.sel$date <- as.Date(model.card.sel$date)
model.card.sel$amount <- as.numeric(as.character(model.card.sel$amount))/1000

model.card.sel$avg_amt_7days <- as.numeric(as.character(model.card.sel$avg_amt_7days))/1000
model.card.sel$tot_amt_7days <- as.numeric(as.character(model.card.sel$tot_amt_7days))/1000

model.card.sel$avg_amt_3days <- as.numeric(as.character(model.card.sel$avg_amt_3days))/1000
model.card.sel$tot_amt_3days <- as.numeric(as.character(model.card.sel$tot_amt_3days))/1000

model.card.sel$avg_amt_1days <- as.numeric(as.character(model.card.sel$avg_amt_1days))/1000
model.card.sel$tot_amt_1days <- as.numeric(as.character(model.card.sel$tot_amt_1days))/1000

model.card.sel$avg_amt_7days_merch <- as.numeric(as.character(model.card.sel$avg_amt_7days_merch))/1000
model.card.sel$tot_amt_7days_merch <- as.numeric(as.character(model.card.sel$tot_amt_7days_merch))/1000

model.card.sel$avg_amt_3days_merch <- as.numeric(as.character(model.card.sel$avg_amt_3days_merch))/1000
model.card.sel$tot_amt_3days_merch <- as.numeric(as.character(model.card.sel$tot_amt_3days_merch))/1000

model.card.sel$avg_amt_1days_merch <- as.numeric(as.character(model.card.sel$avg_amt_1days_merch))/1000
model.card.sel$tot_amt_1days_merch <- as.numeric(as.character(model.card.sel$tot_amt_1days_merch))/1000

model.card.sel$ratio.avg_amt_7days <- ifelse(model.card.sel$avg_amt_7days == 0, 0, model.card.sel$amount/model.card.sel$avg_amt_7days)
model.card.sel$ratio.avg_amt_3days <- ifelse(model.card.sel$avg_amt_3days == 0, 0, model.card.sel$amount/model.card.sel$avg_amt_3days)
model.card.sel$ratio.avg_amt_1days <- ifelse(model.card.sel$avg_amt_1days == 0, 0, model.card.sel$amount/model.card.sel$avg_amt_1days)

model.card.sel$ratio.avg_amt_7days_merch <- ifelse(model.card.sel$avg_amt_7days_merch == 0, 0, model.card.sel$amount/model.card.sel$avg_amt_7days_merch)
model.card.sel$ratio.avg_amt_3days_merch <- ifelse(model.card.sel$avg_amt_3days_merch == 0, 0, model.card.sel$amount/model.card.sel$avg_amt_3days_merch)
model.card.sel$ratio.avg_amt_1days_merch <- ifelse(model.card.sel$avg_amt_1days_merch == 0, 0, model.card.sel$amount/model.card.sel$avg_amt_1days_merch)

set.seed(123)

model.card.sel.intime <- model.card.sel %>% filter(date<=as.Date('2010-09-30')) 
model.card.sel.outoftime <- model.card.sel %>% filter(date>as.Date('2010-09-30')) 
d <- sort(sample(nrow(model.card.sel.intime), nrow(model.card.sel.intime)*0.7))
dev<-model.card.sel.intime[d,]
val<-model.card.sel.intime[-d,]
dev$training <- 1
val$training <- 0
model.customers <- rbind(dev,val)
eval_dat <- select(model.customers, fraud, training)

colnames(dev)

write.csv(dev,"dev.csv")

# rvar <- "fraud"
# evar <- c("transtype","amount","no_tx_7days","avg_amt_7days","tot_amt_7days","tot_days_7days","uzip_7","umerch_7","no_tx_3days",
#                  "tot_days_3days","uzip_3","umerch_3","no_tx_1days","avg_amt_1days","tot_amt_1days",
#                  "umerch_1","no_tx_7days_merch","avg_amt_7days_merch","tot_amt_7days_merch","tot_days_7days_merch",
#                  "uzip_7_merch","umerch_7_merch","no_tx_3days_merch","avg_amt_3days_merch","tot_amt_3days_merch",
#                  "tot_days_3days_merch","uzip_3_merch" , "umerch_3_merch","no_tx_1days_merch" ,
#                  "avg_amt_1days_merch","tot_amt_1days_merch","uzip_1_merch","umerch_1_merch",
#           "ratio.avg_amt_7days","ratio.avg_amt_3days","ratio.avg_amt_1days",
#           "ratio.avg_amt_7days_merch", "ratio.avg_amt_3days_merch", "ratio.avg_amt_1days_merch")
# evar <- c('no_tx_7days','avg_amt_7days','tot_amt_7days','tot_days_7days','uzip_7','umerch_7','no_tx_3days','avg_amt_3days','tot_amt_3days','tot_days_3days','uzip_3','umerch_3','no_tx_1days','avg_amt_1days','tot_amt_1days','tot_days_1days','uzip_1','umerch_1','no_tx_7days_merch','avg_amt_7days_merch','tot_amt_7days_merch','tot_days_7days_merch','uzip_7_merch','umerch_7_merch','no_tx_3days_merch','avg_amt_3days_merch','tot_amt_3days_merch','tot_days_3days_merch','uzip_3_merch','umerch_3_merch','no_tx_1days_merch','avg_amt_1days_merch','tot_amt_1days_merch','tot_days_1days_merch','uzip_1_merch','umerch_1_merch')


#### Logistic - Stepwise
# result <- logistic(dataset = "dev", rvar = rvar, evar = evar, lev = "1",check = "stepwise-backward")
# summary(result, sum_check = c("vif", "confint", "odds"))


# evar <- c("amount","no_tx_7days","tot_days_7days",
#           "umerch_7","no_tx_3days","tot_days_3days","avg_amt_1days","tot_amt_1days",
#           "no_tx_7days_merch","tot_days_7days_merch",
#           "avg_amt_3days_merch","tot_amt_3days_merch","no_tx_1days_merch","avg_amt_1days_merch","tot_amt_1days_merch",
#            "ratio.avg_amt_7days", "ratio.avg_amt_3days", 
#             "ratio.avg_amt_1days" , "ratio.avg_amt_1days_merch")
# "uzip_1_merch" , "umerch_1_merch" ,
# fraud ~ amount + no_tx_7days + tot_days_7days + umerch_7 + no_tx_3days +
#   tot_days_3days + avg_amt_1days + tot_amt_1days + no_tx_7days_merch +
#   tot_days_7days_merch + avg_amt_3days_merch + tot_amt_3days_merch +
#   no_tx_1days_merch + avg_amt_1days_merch + tot_amt_1days_merch +
#   uzip_1_merch + umerch_1_merch + ratio.avg_amt_7days + ratio.avg_amt_3days +
#   ratio.avg_amt_1days + ratio.avg_amt_1days_merch

rvar <- "fraud"
evar <- c("amount", "no_tx_7days", "tot_days_7days", "umerch_7", "avg_amt_1days", "tot_amt_1days", 
          "no_tx_7days_merch", "tot_days_7days_merch", "avg_amt_3days_merch", 
          "avg_amt_1days_merch", "tot_amt_1days_merch", "ratio.avg_amt_7days", "ratio.avg_amt_3days", 
          "ratio.avg_amt_1days", "ratio.avg_amt_1days_merch")

#### Logistic
result.logit <- logistic(dataset = "dev", rvar = rvar, evar = evar, lev = "1")
summary(result.logit, sum_check = c("vif", "confint", "odds"))
plot(result.logit, plots = "coef", custom = FALSE)
pred <- predict(result.logit, pred_data = "model.customers")
eval_dat$logit <- pred[["Prediction"]]

evar <- c("amount", "no_tx_7days", "umerch_7", "avg_amt_1days", "tot_amt_1days", "no_tx_7days_merch", 
          "tot_days_7days_merch", "avg_amt_3days_merch", "tot_amt_1days_merch", "ratio.avg_amt_7days", 
          "ratio.avg_amt_3days", "ratio.avg_amt_1days", "ratio.avg_amt_1days_merch")
int <- c("no_tx_7days:umerch_7", "no_tx_7days:avg_amt_1days", "no_tx_7days:ratio.avg_amt_1days", 
         "umerch_7:tot_amt_1days", "avg_amt_1days:tot_amt_1days", "tot_amt_1days:tot_amt_1days_merch")

result.logit.int <- logistic(dataset = "dev", rvar = rvar, evar = evar, lev = "1",int = int)
summary(result.logit.int, sum_check = c("vif", "confint", "odds"))
plot(result.logit.int, plots = "coef", custom = FALSE)
pred <- predict(result.logit.int, pred_data = "model.customers")
eval_dat$logit.int <- pred[["Prediction"]]

## ANN

result.ann2 <- ann(dev, rvar = rvar, evar = evar, lev = "1", size = 2, decay = 0.25, seed = 1234)
# summary(result.ann2)
plot(result.ann2, plots = "olden", custom = FALSE)
pred <- predict(result.ann2, pred_data = "model.customers")
# print(pred, n = 10)
eval_dat$annr.2 <- pred[["Prediction"]]

result.ann3 <- ann(dev, rvar = rvar, evar = evar, lev = "1", size = 3, decay = 0.01, seed = 1234)
# summary(result.ann3)
plot(result.ann3, plots = "olden", custom = FALSE)
pred <- predict(result.ann3, pred_data = "model.customers")
# print(pred, n = 10)
eval_dat$annr.3 <- pred[["Prediction"]]

result.ann4 <- ann(dev, rvar = rvar, evar = evar, lev = "1", size = 4, decay = 0.01, seed = 1234)
# summary(result.ann4)
plot(result.ann4, plots = "olden", custom = FALSE)
pred <- predict(result.ann4, pred_data = "model.customers")
# print(pred, n = 10)
eval_dat$annr.4 <- pred[["Prediction"]]


####### Model Performance
#### AUC curve - Logistic Regression
prob.dev <- eval_dat[eval_dat$training == 1,]$logit # predicted probabilities
pred.dev <- prediction(prob.dev,eval_dat[eval_dat$training == 1,]$fraud)
perf.dev <- performance(pred.dev, measure = "tpr", x.measure = "fpr")
?performance
prob.val <- eval_dat[eval_dat$training == 0,]$logit # predicted probabilities
pred.val <- prediction(prob.val,eval_dat[eval_dat$training == 0,]$fraud)
perf.val <- performance(pred.val,measure = "tpr", x.measure = "fpr")


# Out of time validation
model.card.sel.outoftime.pred <- model.card.sel.outoftime
pred.oot <- predict(result.logit, pred_data = "model.card.sel.outoftime")
model.card.sel.outoftime.pred$logit <- pred.oot[["Prediction"]]
prob.oot <- model.card.sel.outoftime.pred$logit # predicted probabilities
pred.oot <- prediction(prob.oot,model.card.sel.outoftime.pred$fraud)
perf.oot <- performance(pred.oot,measure = "tpr",x.measure = "fpr")

plot(perf.dev, col ="red")
plot(perf.val, add = TRUE, colorize = TRUE)
plot(perf.oot, add = TRUE, col ="green")

auc.dev <- performance(pred.dev, measure = "auc")
auc.dev <- auc.dev@y.values[[1]]
auc.dev
auc.val <- performance(pred.val, measure = "auc")
auc.val <- auc.val@y.values[[1]]
auc.val
auc.oot <- performance(pred.oot, measure = "auc")
auc.oot <- auc.oot@y.values[[1]]
auc.oot

## KS Statistics
max(attr(perf.dev,'y.values')[[1]]-attr(perf.dev,'x.values')[[1]])
max(attr(perf.val,'y.values')[[1]]-attr(perf.val,'x.values')[[1]])
max(attr(perf.oot,'y.values')[[1]]-attr(perf.oot,'x.values')[[1]])

#######confusion matrix##
conf_data<-as.data.frame(cbind(prob.dev, eval_dat[eval_dat$training == 1,]$fraud))
table(conf_data$prob.dev>mean(eval_dat$fraud=="1"),eval_dat[eval_dat$training == 1,]$fraud)
m1<-table(conf_data$prob.dev>mean(eval_dat[eval_dat$training == 1,]$fraud=="1"),eval_dat[eval_dat$training == 1,]$fraud)
accuracy.logistic.dev <-(m1[1,1]+m1[2,2])/(m1[1,1]+m1[1,2]+m1[2,2]+m1[2,1])

#######confusion matrix##
conf_data<-as.data.frame(cbind(prob.val, eval_dat[eval_dat$training == 0,]$fraud))
table(conf_data$prob.val>mean(eval_dat$fraud=="1"),eval_dat[eval_dat$training == 0,]$fraud)
m2<-table(conf_data$prob.val>mean(eval_dat$fraud=="1"),eval_dat[eval_dat$training == 0,]$fraud)
accuracy.logistic.val <-(m2[1,1]+m2[2,2])/(m2[1,1]+m2[1,2]+m2[2,2]+m2[2,1])

conf_data<-as.data.frame(cbind(prob.oot, model.card.sel.outoftime.pred$fraud))
table(conf_data$prob.oot>mean(eval_dat$fraud=="1"),model.card.sel.outoftime.pred$fraud)
m2<-table(conf_data$prob.oot>mean(eval_dat$fraud=="1"),model.card.sel.outoftime.pred$fraud)
accuracy.logistic.oot <-(m2[1,1]+m2[2,2])/(m2[1,1]+m2[1,2]+m2[2,2]+m2[2,1])

accuracy.logistic.dev
accuracy.logistic.val
accuracy.logistic.oot



####### Model Performance
#### AUC curve - Logistic Regression with interaction
prob.dev <- eval_dat[eval_dat$training == 1,]$logit.int # predicted probabilities
pred.dev <- prediction(prob.dev,eval_dat[eval_dat$training == 1,]$fraud)
perf.dev <- performance(pred.dev,measure = "tpr",x.measure = "fpr")

prob.val <- eval_dat[eval_dat$training == 0,]$logit.int # predicted probabilities
pred.val <- prediction(prob.val,eval_dat[eval_dat$training == 0,]$fraud)
perf.val <- performance(pred.val,measure = "tpr",x.measure = "fpr")

# Out of time validation
pred.oot <- predict(result.logit.int, pred_data = "model.card.sel.outoftime")
model.card.sel.outoftime.pred$logit.int <- pred.oot[["Prediction"]]
prob.oot <- model.card.sel.outoftime.pred$logit.int # predicted probabilities
pred.oot <- prediction(prob.oot,model.card.sel.outoftime.pred$fraud)
perf.oot <- performance(pred.oot,measure = "tpr",x.measure = "fpr")

plot(perf.dev, col ="red")
plot(perf.val, add = TRUE, colorize = TRUE)
plot(perf.oot, add = TRUE, col ="green")


auc.dev <- performance(pred.dev, measure = "auc")
auc.dev <- auc.dev@y.values[[1]]
auc.dev
auc.val <- performance(pred.val, measure = "auc")
auc.val <- auc.val@y.values[[1]]
auc.val
auc.oot <- performance(pred.oot, measure = "auc")
auc.oot <- auc.oot@y.values[[1]]
auc.oot

## KS Statistics
max(attr(perf.dev,'y.values')[[1]]-attr(perf.dev,'x.values')[[1]])
max(attr(perf.val,'y.values')[[1]]-attr(perf.val,'x.values')[[1]])
max(attr(perf.oot,'y.values')[[1]]-attr(perf.oot,'x.values')[[1]])



#######confusion matrix##
conf_data<-as.data.frame(cbind(prob.dev, eval_dat[eval_dat$training == 1,]$fraud))
table(conf_data$prob.dev>mean(eval_dat$fraud=="1"),eval_dat[eval_dat$training == 1,]$fraud)
m1<-table(conf_data$prob.dev>mean(eval_dat[eval_dat$training == 1,]$fraud=="1"),eval_dat[eval_dat$training == 1,]$fraud)
accuracy.logistic.int.dev <-(m1[1,1]+m1[2,2])/(m1[1,1]+m1[1,2]+m1[2,2]+m1[2,1])

#######confusion matrix##
conf_data<-as.data.frame(cbind(prob.val, eval_dat[eval_dat$training == 0,]$fraud))
table(conf_data$prob.val>mean(eval_dat$fraud=="1"),eval_dat[eval_dat$training == 0,]$fraud)
m2<-table(conf_data$prob.val>mean(eval_dat$fraud=="1"),eval_dat[eval_dat$training == 0,]$fraud)
accuracy.logistic.int.val <-(m2[1,1]+m2[2,2])/(m2[1,1]+m2[1,2]+m2[2,2]+m2[2,1])

conf_data<-as.data.frame(cbind(prob.oot, model.card.sel.outoftime.pred$fraud))
table(conf_data$prob.oot>mean(eval_dat$fraud=="1"),model.card.sel.outoftime.pred$fraud)
m2<-table(conf_data$prob.oot>mean(eval_dat$fraud=="1"),model.card.sel.outoftime.pred$fraud)
accuracy.logistic.int.oot <-(m2[1,1]+m2[2,2])/(m2[1,1]+m2[1,2]+m2[2,2]+m2[2,1])


accuracy.logistic.int.dev
accuracy.logistic.int.val
accuracy.logistic.int.oot

#### AUC curve - Neural Net with 2 node
prob.dev <- eval_dat[eval_dat$training == 1,]$annr.2 # predicted probabilities
pred.dev <- prediction(prob.dev,eval_dat[eval_dat$training == 1,]$fraud)
perf.dev <- performance(pred.dev,measure = "tpr",x.measure = "fpr")

prob.val <- eval_dat[eval_dat$training == 0,]$annr.2 # predicted probabilities
pred.val <- prediction(prob.val,eval_dat[eval_dat$training == 0,]$fraud)
perf.val <- performance(pred.val,measure = "tpr",x.measure = "fpr")


# Out of time validation
pred.oot <- predict(result.ann2, pred_data = "model.card.sel.outoftime")
model.card.sel.outoftime.pred$ann2 <- pred.oot[["Prediction"]]
prob.oot <- model.card.sel.outoftime.pred$ann2 # predicted probabilities
pred.oot <- prediction(prob.oot,model.card.sel.outoftime.pred$fraud)
perf.oot <- performance(pred.oot,measure = "tpr",x.measure = "fpr")

plot(perf.dev, col ="red")
plot(perf.val, add = TRUE, colorize = TRUE)
plot(perf.oot, add = TRUE, col ="green")

auc.dev <- performance(pred.dev, measure = "auc")
auc.dev <- auc.dev@y.values[[1]]
auc.dev
auc.val <- performance(pred.val, measure = "auc")
auc.val <- auc.val@y.values[[1]]
auc.val
auc.oot <- performance(pred.oot, measure = "auc")
auc.oot <- auc.oot@y.values[[1]]
auc.oot

## KS Statistics
max(attr(perf.dev,'y.values')[[1]]-attr(perf.dev,'x.values')[[1]])
max(attr(perf.val,'y.values')[[1]]-attr(perf.val,'x.values')[[1]])
max(attr(perf.oot,'y.values')[[1]]-attr(perf.oot,'x.values')[[1]])


#######confusion matrix##
conf_data<-as.data.frame(cbind(prob.dev, eval_dat[eval_dat$training == 1,]$fraud))
table(conf_data$prob.dev>mean(eval_dat[eval_dat$training == 1,]$fraud=="1"),eval_dat[eval_dat$training == 1,]$fraud)
m1<-table(conf_data$prob.dev>mean(eval_dat[eval_dat$training == 1,]$fraud=="1"),eval_dat[eval_dat$training == 1,]$fraud)
accuracy.logistic.dev <-(m1[1,1]+m1[2,2])/(m1[1,1]+m1[1,2]+m1[2,2]+m1[2,1])

#######confusion matrix##
conf_data<-as.data.frame(cbind(prob.val, eval_dat[eval_dat$training == 0,]$fraud))
table(conf_data$prob.val>mean(eval_dat[eval_dat$training == 0,]$fraud=="1"),eval_dat[eval_dat$training == 0,]$fraud)
m2<-table(conf_data$prob.val>mean(eval_dat[eval_dat$training == 0,]$fraud=="1"),eval_dat[eval_dat$training == 0,]$fraud)
accuracy.logistic.val <- (m2[1,1]+m2[2,2])/(m2[1,1]+m2[1,2]+m2[2,2]+m2[2,1])

conf_data<-as.data.frame(cbind(prob.oot, model.card.sel.outoftime.pred$fraud))
table(conf_data$prob.oot>mean(eval_dat$fraud=="1"),model.card.sel.outoftime.pred$fraud)
m2<-table(conf_data$prob.oot>mean(eval_dat$fraud=="1"),model.card.sel.outoftime.pred$fraud)
accuracy.logistic.oot <-(m2[1,1]+m2[2,2])/(m2[1,1]+m2[1,2]+m2[2,2]+m2[2,1])

accuracy.logistic.dev
accuracy.logistic.val
accuracy.logistic.oot


#### AUC curve - Neural Net with 3 node

prob.dev <- eval_dat[eval_dat$training == 1,]$annr.3 # predicted probabilities
pred.dev <- prediction(prob.dev,eval_dat[eval_dat$training == 1,]$fraud)
perf.dev <- performance(pred.dev,measure = "tpr",x.measure = "fpr")

prob.val <- eval_dat[eval_dat$training == 0,]$annr.3 # predicted probabilities
pred.val <- prediction(prob.val,eval_dat[eval_dat$training == 0,]$fraud)
perf.val <- performance(pred.val,measure = "tpr",x.measure = "fpr")
plot(perf.dev, col ="red")
plot(perf.val, add = TRUE, colorize = TRUE)

# Out of time validation
pred.oot <- predict(result.ann3, pred_data = "model.card.sel.outoftime")
model.card.sel.outoftime.pred$ann3 <- pred.oot[["Prediction"]]
prob.oot <- model.card.sel.outoftime.pred$ann3 # predicted probabilities
pred.oot <- prediction(prob.oot,model.card.sel.outoftime.pred$fraud)
perf.oot <- performance(pred.oot,measure = "tpr",x.measure = "fpr")
plot(perf.dev, col ="red")
plot(perf.oot, add = TRUE, colorize = TRUE)

auc.dev <- performance(pred.dev, measure = "auc")
auc.dev <- auc.dev@y.values[[1]]
auc.dev
auc.val <- performance(pred.val, measure = "auc")
auc.val <- auc.val@y.values[[1]]
auc.val
auc.oot <- performance(pred.oot, measure = "auc")
auc.oot <- auc.oot@y.values[[1]]
auc.oot

## KS Statistics
max(attr(perf.dev,'y.values')[[1]]-attr(perf.dev,'x.values')[[1]])
max(attr(perf.val,'y.values')[[1]]-attr(perf.val,'x.values')[[1]])
max(attr(perf.oot,'y.values')[[1]]-attr(perf.oot,'x.values')[[1]])


#######confusion matrix##
conf_data<-as.data.frame(cbind(prob.dev, eval_dat[eval_dat$training == 1,]$fraud))
table(conf_data$prob.dev>mean(eval_dat[eval_dat$training == 1,]$fraud=="1"),eval_dat[eval_dat$training == 1,]$fraud)
m1<-table(conf_data$prob.dev>mean(eval_dat[eval_dat$training == 1,]$fraud=="1"),eval_dat[eval_dat$training == 1,]$fraud)
accuracy.logistic.dev <-(m1[1,1]+m1[2,2])/(m1[1,1]+m1[1,2]+m1[2,2]+m1[2,1])


#######confusion matrix##
conf_data<-as.data.frame(cbind(prob.val, eval_dat[eval_dat$training == 0,]$fraud))
table(conf_data$prob.val>mean(eval_dat[eval_dat$training == 0,]$fraud=="1"),eval_dat[eval_dat$training == 0,]$fraud)
m2<-table(conf_data$prob.val>mean(eval_dat[eval_dat$training == 0,]$fraud=="1"),eval_dat[eval_dat$training == 0,]$fraud)
accuracy.logistic.val <- (m2[1,1]+m2[2,2])/(m2[1,1]+m2[1,2]+m2[2,2]+m2[2,1])

conf_data<-as.data.frame(cbind(prob.oot, model.card.sel.outoftime.pred$fraud))
table(conf_data$prob.oot>mean(eval_dat$fraud=="1"),model.card.sel.outoftime.pred$fraud)
m2<-table(conf_data$prob.oot>mean(eval_dat$fraud=="1"),model.card.sel.outoftime.pred$fraud)
accuracy.logistic.oot <-(m2[1,1]+m2[2,2])/(m2[1,1]+m2[1,2]+m2[2,2]+m2[2,1])


accuracy.logistic.dev
accuracy.logistic.val
accuracy.logistic.oot

#### AUC curve - Neural Net with 4 node

prob.dev <- eval_dat[eval_dat$training == 1,]$annr.4 # predicted probabilities
pred.dev <- prediction(prob.dev,eval_dat[eval_dat$training == 1,]$fraud)
perf.dev <- performance(pred.dev, measure = "tpr", x.measure = "fpr")


prob.val <- eval_dat[eval_dat$training == 0,]$annr.4 # predicted probabilities
pred.val <- prediction(prob.val,eval_dat[eval_dat$training == 0,]$fraud)
perf.val <- performance(pred.val,measure = "tpr",x.measure = "fpr")

# Out of time validation
pred.oot <- predict(result.ann4, pred_data = "model.card.sel.outoftime")
model.card.sel.outoftime.pred$ann4 <- pred.oot[["Prediction"]]
prob.oot <- model.card.sel.outoftime.pred$ann4 # predicted probabilities
pred.oot <- prediction(prob.oot,model.card.sel.outoftime.pred$fraud)
perf.oot <- performance(pred.oot,measure = "tpr",x.measure = "fpr")

plot(perf.dev, col ="red")
plot(perf.val, add = TRUE, colorize = TRUE)
plot(perf.oot, add = TRUE, col ="green")


auc.dev <- performance(pred.dev, measure = "auc")
auc.dev <- auc.dev@y.values[[1]]
auc.dev
auc.val <- performance(pred.val, measure = "auc")
auc.val <- auc.val@y.values[[1]]
auc.val
auc.oot <- performance(pred.oot, measure = "auc")
auc.oot <- auc.oot@y.values[[1]]
auc.oot

## KS Statistics
max(attr(perf.dev,'y.values')[[1]]-attr(perf.dev,'x.values')[[1]])
max(attr(perf.val,'y.values')[[1]]-attr(perf.val,'x.values')[[1]])
max(attr(perf.oot,'y.values')[[1]]-attr(perf.oot,'x.values')[[1]])

#######confusion matrix##
conf_data<-as.data.frame(cbind(prob.dev, eval_dat[eval_dat$training == 1,]$fraud))
table(conf_data$prob.dev>mean(eval_dat[eval_dat$training == 1,]$fraud=="1"),eval_dat[eval_dat$training == 1,]$fraud)
m1<-table(conf_data$prob.dev>mean(eval_dat[eval_dat$training == 1,]$fraud=="1"),eval_dat[eval_dat$training == 1,]$fraud)
accuracy.logistic.dev <-(m1[1,1]+m1[2,2])/(m1[1,1]+m1[1,2]+m1[2,2]+m1[2,1])
accuracy.logistic.dev 

#######confusion matrix##
conf_data<-as.data.frame(cbind(prob.val, eval_dat[eval_dat$training == 0,]$fraud))
table(conf_data$prob.val>mean(eval_dat[eval_dat$training == 0,]$fraud=="1"),eval_dat[eval_dat$training == 0,]$fraud)
m2<-table(conf_data$prob.val>mean(eval_dat[eval_dat$training == 0,]$fraud=="1"),eval_dat[eval_dat$training == 0,]$fraud)
accuracy.logistic.val <- (m2[1,1]+m2[2,2])/(m2[1,1]+m2[1,2]+m2[2,2]+m2[2,1])

conf_data<-as.data.frame(cbind(prob.oot, model.card.sel.outoftime.pred$fraud))
table(conf_data$prob.oot>mean(eval_dat$fraud=="1"),model.card.sel.outoftime.pred$fraud)
m2<-table(conf_data$prob.oot>mean(eval_dat$fraud=="1"),model.card.sel.outoftime.pred$fraud)
accuracy.logistic.oot <-(m2[1,1]+m2[2,2])/(m2[1,1]+m2[1,2]+m2[2,2]+m2[2,1])

accuracy.logistic.dev
accuracy.logistic.val
accuracy.logistic.oot


## evaluate gain curve
mods <- colnames(eval_dat)[-1:-2]
result <- evalbin(dataset = "eval_dat", pred = "logit", rvar = rvar, lev = "1",
                  qnt = 100, train = "Both", data_filter = "training == 1")
plot(result, plots = "gains")

result <- evalbin(dataset = "eval_dat", pred = "annr.2", rvar = rvar, lev = "1",
                  qnt = 100, train = "Both", data_filter = "training == 1")
plot(result, plots = "gains")

result <- evalbin(dataset = "eval_dat", pred = "annr.3", rvar = rvar, lev = "1",
                  qnt = 100, train = "Both", data_filter = "training == 1")
plot(result, plots = "gains")

write.csv(model.card.sel.outoftime.pred,"out_of_sample_validation.csv")


######################################################################## end ######################





