###################################################################################
###### STAT 557 (Group Project 2); Muhammed Cifci, Caner Simsek, Chase Bloch ######
###################################################################################
library(readxl)

RNGkind(sample.kind = "Rounding")
set.seed(1908)

data <- read_excel("data.xlsx")

#id <- sample(seq(1, 2), size = nrow(data), replace = TRUE, prob = c(.75, .25))
#train_x <- data[id == 1,]
#test_x <- data[id == 2,]
train_x_final <- subset(train, select = c(efficacy,economic,cultural,eduyrs,agea,gndr,hinctnta,cntry,populist))
test_x_final <- subset(test, select = c(efficacy,economic,cultural,eduyrs,agea,gndr,hinctnta,cntry,populist))

#install.packages("xgboost")
library(xgboost)

start_time <- Sys.time()

library(mlr)
#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="error", nrounds=100L, eta=0.1)

params <- makeParamSet(makeDiscreteParam("booster",values = c("gbtree","gblinear")), makeIntegerParam("max_depth",lower = 3L,upper = 10L), makeNumericParam("min_child_weight",lower = 1L,upper = 10L), makeNumericParam("subsample",lower = 0.5,upper = 1), makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
library(parallel)
library(parallelMap) 
#parallelStartSocket(cpus = 4)

#parameter tuning
train_x_final$populist <- as.factor(train_x_final$populist)
test_x_final$populist <- as.factor(test_x_final$populist)


dummy <- dummyVars("~.",data=train_x_final)
train_x_final_2 <- data.frame(predict(dummy,newdata=train_x_final))


traintask <- makeClassifTask(data = train_x_final,target = "populist")
testtask <- makeClassifTask(data = test_x_final,target = "populist")

mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)
mytune$y 

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- train(learner = lrn_tune,task = traintask)

pred <- as.numeric(predict(xgmodel, testtask)[[2]][[3]])-1
truth <- as.numeric(predict(xgmodel, testtask)[[2]][[2]])-1

end_time <- Sys.time()
#Runtime
runtime <- end_time - start_time

table(pred, truth)

#Classification Error Rate
cer <- sum(pred!=truth)/length(pred)

#Train ROC
trpred <- as.numeric(predict(xgmodel, traintask)[[2]][[3]])-1
trtruth <- as.numeric(predict(xgmodel, traintask)[[2]][[2]])-1
table(trpred,trtruth)
plot.roc(trpred,trtruth)

#Test ROC
library(PRROC)

PRROC_obj_train <- roc.curve(scores.class0 = trpred, weights.class0=trtruth,curve=TRUE)
plot(PRROC_obj_train)


PRROC_obj <- roc.curve(scores.class0 = pred, weights.class0=truth,curve=TRUE)
plot(PRROC_obj)

