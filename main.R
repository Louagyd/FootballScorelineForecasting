library(dplyr)
library(rlang)
library(readr)
library(magrittr)
library(ggplot2)
library(GGally)
library(eRic)

# load data

load("datalast/train_all_data2.Rda")
load("datalast/valid_all_data2.Rda")
load("datalast/test_all_data2.Rda")

load("datalast/train_all_targets2.Rda")
load("datalast/valid_all_targets2.Rda")
load("datalast/test_all_targets2.Rda")

#implied probabilities by the bookmakers

probs_implied_train = data.frame("HpstarB" = 1/train_all_targets$B365H,
                                 "DpstarB" = 1/train_all_targets$B365D,
                                 "ApstarB" = 1/train_all_targets$B365A,
                                 "HpstarW" = 1/train_all_targets$WHH,
                                 "DpstarW" = 1/train_all_targets$WHD,
                                 "ApstarW" = 1/train_all_targets$WHA)
library(implied)
odds_B = data.frame(train_all_targets$B365H, train_all_targets$B365D, train_all_targets$B365A)
implied_basic_B = implied_probabilities(odds_B, method = "basic")$probabilities
implied_shin_B = implied_probabilities(odds_B, method = "shin")$probabilities
odds_W = data.frame(train_all_targets$WHH, train_all_targets$WHD, train_all_targets$WHA)
implied_basic_W = implied_probabilities(odds_W, method = "basic")$probabilities
implied_shin_W = implied_probabilities(odds_W, method = "shin")$probabilities
probs_implied_train = cbind(probs_implied_train, implied_basic_B)
probs_implied_train = cbind(probs_implied_train, implied_basic_W)
probs_implied_train = cbind(probs_implied_train, implied_shin_B)
probs_implied_train = cbind(probs_implied_train, implied_shin_W)
colnames(probs_implied_train)[7:18] = c("HbasicB", "DbasicB", "AbasicB", "HshinB", "DshinB", "AshinB", "HbasicW", "DbasicW", "AbasicW", "HshinW", "DshinW", "AshinW")


probs_implied_valid = data.frame("HpstarB" = 1/valid_all_targets$B365H,
                                 "DpstarB" = 1/valid_all_targets$B365D,
                                 "ApstarB" = 1/valid_all_targets$B365A,
                                 "HpstarW" = 1/valid_all_targets$WHH,
                                 "DpstarW" = 1/valid_all_targets$WHD,
                                 "ApstarW" = 1/valid_all_targets$WHA)

odds_B = data.frame(valid_all_targets$B365H, valid_all_targets$B365D, valid_all_targets$B365A)
implied_basic_B = implied_probabilities(odds_B, method = "basic")$probabilities
implied_shin_B = implied_probabilities(odds_B, method = "shin")$probabilities
odds_W = data.frame(valid_all_targets$WHH, valid_all_targets$WHD, valid_all_targets$WHA)
implied_basic_W = implied_probabilities(odds_W, method = "basic")$probabilities
implied_shin_W = implied_probabilities(odds_W, method = "shin")$probabilities
probs_implied_valid = cbind(probs_implied_valid, implied_basic_B)
probs_implied_valid = cbind(probs_implied_valid, implied_basic_W)
probs_implied_valid = cbind(probs_implied_valid, implied_shin_B)
probs_implied_valid = cbind(probs_implied_valid, implied_shin_W)
colnames(probs_implied_valid)[7:18] = c("HbasicB", "DbasicB", "AbasicB", "HshinB", "DshinB", "AshinB", "HbasicW", "DbasicW", "AbasicW", "HshinW", "DshinW", "AshinW")


probs_implied_test = data.frame("HpstarB" = 1/test_all_targets$B365H,
                                 "DpstarB" = 1/test_all_targets$B365D,
                                 "ApstarB" = 1/test_all_targets$B365A,
                                 "HpstarW" = 1/test_all_targets$WHH,
                                 "DpstarW" = 1/test_all_targets$WHD,
                                 "ApstarW" = 1/test_all_targets$WHA)

odds_B = data.frame(test_all_targets$B365H, test_all_targets$B365D, test_all_targets$B365A)
implied_basic_B = implied_probabilities(odds_B, method = "basic")$probabilities
implied_shin_B = implied_probabilities(odds_B, method = "shin")$probabilities
odds_W = data.frame(test_all_targets$WHH, test_all_targets$WHD, test_all_targets$WHA)
implied_basic_W = implied_probabilities(odds_W, method = "basic")$probabilities
implied_shin_W = implied_probabilities(odds_W, method = "shin")$probabilities
probs_implied_test = cbind(probs_implied_test, implied_basic_B)
probs_implied_test = cbind(probs_implied_test, implied_basic_W)
probs_implied_test = cbind(probs_implied_test, implied_shin_B)
probs_implied_test = cbind(probs_implied_test, implied_shin_W)
colnames(probs_implied_test)[7:18] = c("HbasicB", "DbasicB", "AbasicB", "HshinB", "DshinB", "AshinB", "HbasicW", "DbasicW", "AbasicW", "HshinW", "DshinW", "AshinW")


# compute commission

compute_expected_profit = function(pr, res) {
  n = length(pr)
  prf = 1 - (int(res)-1)*(1/pr)
  return(mean(prf))
}

probs_train = data.frame("BbMx" = 1/train_all_targets$Mx25, "BbAv" = 1/train_all_targets$Av25)
probs_valid = data.frame("BbMx" = 1/valid_all_targets$Mx25, "BbAv" = 1/valid_all_targets$Av25)
probs_test = data.frame("BbMx" = 1/test_all_targets$Mx25, "BbAv" = 1/test_all_targets$Av25)

M25target_train = as.numeric(as.factor(train_all_targets$GT>2.5))-1
M25target_valid = as.numeric(as.factor(valid_all_targets$GT>2.5))-1
M25target_test = as.numeric(as.factor(test_all_targets$GT>2.5))-1
 
HDA_target_train = data.frame("H" = as.numeric(as.factor(train_all_targets$FR==1))-1,
                              "D" = as.numeric(as.factor(train_all_targets$FR==0))-1,
                              "A" = as.numeric(as.factor(train_all_targets$FR==-1))-1)
HDA_target_train_classes = as.factor(1*HDA_target_train$H+2*HDA_target_train$D+3*HDA_target_train$A)
HDA_target_valid = data.frame("H" = as.numeric(as.factor(valid_all_targets$FR==1))-1,
                              "D" = as.numeric(as.factor(valid_all_targets$FR==0))-1,
                              "A" = as.numeric(as.factor(valid_all_targets$FR==-1))-1)
HDA_target_valid_classes = as.factor(1*HDA_target_valid$H+2*HDA_target_valid$D+3*HDA_target_valid$A)
HDA_target_test = data.frame("H" = as.numeric(as.factor(test_all_targets$FR==1))-1,
                             "D" = as.numeric(as.factor(test_all_targets$FR==0))-1,
                             "A" = as.numeric(as.factor(test_all_targets$FR==-1))-1)
HDA_target_test_classes = as.factor(1*HDA_target_test$H+2*HDA_target_test$D+3*HDA_target_test$A)

MxProfit = compute_expected_profit(probs_train$BbMx, as.factor(M25target_train))
AvProfit = compute_expected_profit(probs_train$BbAv, as.factor(M25target_train))

probs_train$BbAvC <- (1-AvProfit)*probs_train$BbAv
probs_valid$BbAvC <- (1-AvProfit)*probs_valid$BbAv
probs_test$BbAvC <- (1-AvProfit)*probs_test$BbAv
AvCProfit = compute_expected_profit(probs_train$BbAvC, as.factor(M25target_train))

# Function for evaluating probability estimations

cross.entropy <- function(p, phat){
  x <- 0
  for (i in 1:length(p)){
    x <- x + sum(p[i] * log(phat[i]))/dim(p)[1]
  }
  return(-x)
}
library(verification)
library(MLmetrics)
prob_eval_single = function(target, probs, plot = 0) {
  pairsplot = 0
  if (plot){
    pairsplot = ggpairs(probs, aes(colour = as.factor(target), alpha = 0.04)) + scale_color_manual(values =  c("black", "azure4")) + scale_fill_manual(values =  c("black", "azure4")) + theme_bw()
  }
  briers = list()
  reliabilities = list()
  refinements = list()
  logloss = list()
  for (i in colnames(probs)){
    cc = verify(thresholds = seq(0,1,0.03), obs=target, pred=probs[i][[1]])
  
    briers[i] = cc$bs
    reliabilities[i] = cc$bs.reliability
    refinements[i] = cc$bs.uncert - cc$bs.resol
    logloss[i] = LogLoss(probs[i][[1]], target)
  }
  return(list("briers"=briers, "reliabilities"=reliabilities, "refinements"=refinements, "logloss"=logloss, "plot"=pairsplot))
}

require(reliabilitydiag)
r = reliabilitydiag(
  probs_train[c("BbMx", "BbAv")],
  y = as.integer(train_all_targets$GT > 2.5),
  region.level = NA
)
autoplot(r) + xlim(0.25,0.75) + ylim(0.25,0.75) + scale_color_manual(values =  c("black", "azure4")) + geom_abline(slope = 1, intercept = 0, linetype = "dotted")

prob_eval_multiple = function(target, probs){
  target_classes = as.factor( 1*target$H + 2*target$D + 3*target$A )
  RPSs = list()
  LLs = list()
  for (i in seq(1:((length(probs)/3)))){
    
    this_probs = probs[,(3*i-2):(3*i)]
    LLs[i] = cross.entropy(target, this_probs)
    RPSs[i] = rps(obs=as.matrix(as.integer(target_classes)), pred=as.matrix(this_probs))$rps
    
  }
  return(list("LLs"=LLs, "RPSs"=RPSs))
}

# Evaluating Bookmakers

prob_eval_single(M25target_train, probs_train)
prob_eval_multiple(HDA_target_train, probs_implied_train)


# Logistic Regression

logistic_model = glm(as.factor(`train_all_targets$GT` > 2.5) ~ ., data = cbind(train_all_data, train_all_targets$GT), family = "binomial")

probs_train$lr = predict(logistic_model, train_all_data, type = "response")
probs_valid$lr = predict(logistic_model, valid_all_data, type = "response")
probs_test$lr = predict(logistic_model, test_all_data, type = "response")

#MULTICLASS

library(nnet)
multinom_model = multinom(as.factor(`train_all_targets$FR`) ~ ., data = cbind(train_all_data, train_all_targets$FR))
mmtrain = predict(multinom_model, train_all_data, type = "probs")
probs_implied_train$mnH = mmtrain[,3]
probs_implied_train$mnD = mmtrain[,2]
probs_implied_train$mnA = mmtrain[,1]
mmvalid = predict(multinom_model, valid_all_data, type = "probs")
probs_implied_valid$mnH = mmvalid[,3]
probs_implied_valid$mnD = mmvalid[,2]
probs_implied_valid$mnA = mmvalid[,1]
mmtest = predict(multinom_model, test_all_data, type = "probs")
probs_implied_test$mnH = mmtest[,3]
probs_implied_test$mnD = mmtest[,2]
probs_implied_test$mnA = mmtest[,1]



# Random Forest
require(randomForest)
hps = expand.grid((seq(15,15,10)), (seq(400, 400, 200)))
colnames(hps) = c("max_nodes", "num_trees")
for (i in seq(1:dim(hps)[1])){
  rf0 = randomForest(as.factor(`train_all_targets$GT` > 2.5) ~ .,data=cbind(train_all_data, train_all_targets$GT), maxnodes=hps$max_nodes[i], ntree=hps$num_trees[i])
  
  probs_train$rf = predict(rf0, train_all_data, type="prob")[,2]
  probs_valid$rf = predict(rf0, valid_all_data, type="prob")[,2]
  probs_valid$rfc <- prCalibrate(M25target_train, probs_train$rf,
                                 M25target_valid, probs_valid$rf, nbins = 30)$cal.probs
  this_loss = LogLoss(probs_valid$rfc, M25target_valid)
  hps$loss[i] = this_loss
  print(as.integer(i/dim(hps)[1]*100))
}
ggplot(hps, aes(max_nodes, num_trees, fill= loss)) + 
  geom_tile() + scale_x_continuous(breaks = seq(5,45,10)) + scale_y_continuous(breaks = seq(100,1000,200)) + scale_fill_gradient(low="black", high="white")
best_var1 = hps$max_nodes[hps$loss==min(hps$loss)]
best_var2 = hps$num_trees[hps$loss==min(hps$loss)]
rf0 = randomForest(as.factor(`train_all_targets$GT` > 2.5) ~ .,data=cbind(train_all_data, train_all_targets$GT), maxnodes=best_var1, ntree=best_var2)

tmp = data.frame(Feature = rownames(rf0$importance), Val = rf0$importance)
tmp$Feature = factor(tmp$Feature, levels = tmp$Feature)
ggplot(data = tmp, aes(y=MeanDecreaseGini, x= Feature)) + geom_bar(stat="identity") + theme_bw() + scale_y_continuous(name = "Importance")

probs_train$rf = predict(rf0, train_all_data, type="prob")[,2]
probs_valid$rf = predict(rf0, valid_all_data, type="prob")[,2]
probs_test$rf = predict(rf0, test_all_data, type="prob")[,2]

probs_train$rfc <- prCalibrate(M25target_train, probs_train$rf,
                               M25target_train, probs_train$rf, nbins = 30)$cal.probs
probs_valid$rfc <- prCalibrate(M25target_valid, probs_valid$rf,
                               M25target_valid, probs_valid$rf, nbins = 30)$cal.probs
probs_test$rfc <- prCalibrate(M25target_test, probs_test$rf,
                               M25target_test, probs_test$rf, nbins = 30)$cal.probs

# MULTICLASS

rf1 = randomForest(as.factor(`train_all_targets$FR`) ~ .,data=cbind(train_all_data, train_all_targets$FR), maxnodes=best_var1, ntree=best_var2)
plot(rf1)


rftrain = predict(rf1, train_all_data, type="prob")
probs_implied_train$rfH = rftrain[,3]
probs_implied_train$rfD = rftrain[,2]
probs_implied_train$rfA = rftrain[,1]
probs_implied_train$rfcH = prCalibrate(HDA_target_train$H, as.numeric(rftrain[,3]),
                                       HDA_target_train$H, rftrain[,3], nbins = 30)$cal.probs
probs_implied_train$rfcD = prCalibrate(HDA_target_train$D, as.numeric(rftrain[,2]),
                                       HDA_target_train$D, rftrain[,2], nbins = 30)$cal.probs
probs_implied_train$rfcA = prCalibrate(HDA_target_train$A, as.numeric(rftrain[,1]),
                                       HDA_target_train$A, rftrain[,1], nbins = 30)$cal.probs
probs_implied_train$rfcH = probs_implied_train$rfcH / (probs_implied_train$rfcH+probs_implied_train$rfcD+probs_implied_train$rfcA)
probs_implied_train$rfcD = probs_implied_train$rfcD / (probs_implied_train$rfcH+probs_implied_train$rfcD+probs_implied_train$rfcA)
probs_implied_train$rfcA = probs_implied_train$rfcA / (probs_implied_train$rfcH+probs_implied_train$rfcD+probs_implied_train$rfcA)



rfvalid = predict(rf1, valid_all_data, type="prob")
probs_implied_valid$rfH = rfvalid[,3]
probs_implied_valid$rfD = rfvalid[,2]
probs_implied_valid$rfA = rfvalid[,1]
probs_implied_valid$rfcH = prCalibrate(HDA_target_train$H, as.numeric(rftrain[,3]),
                                       HDA_target_valid$H, rfvalid[,3], nbins = 30)$cal.probs
probs_implied_valid$rfcD = prCalibrate(HDA_target_train$D, as.numeric(rftrain[,2]),
                                       HDA_target_valid$D, rfvalid[,2], nbins = 30)$cal.probs
probs_implied_valid$rfcA = prCalibrate(HDA_target_train$A, as.numeric(rftrain[,1]),
                                       HDA_target_valid$A, rfvalid[,1], nbins = 30)$cal.probs
probs_implied_valid$rfcH = probs_implied_valid$rfcH / (probs_implied_valid$rfcH+probs_implied_valid$rfcD+probs_implied_valid$rfcA)
probs_implied_valid$rfcD = probs_implied_valid$rfcD / (probs_implied_valid$rfcH+probs_implied_valid$rfcD+probs_implied_valid$rfcA)
probs_implied_valid$rfcA = probs_implied_valid$rfcA / (probs_implied_valid$rfcH+probs_implied_valid$rfcD+probs_implied_valid$rfcA)




rftest = predict(rf1, test_all_data, type = "prob")
probs_implied_test$rfH = rftest[,3]
probs_implied_test$rfD = rftest[,2]
probs_implied_test$rfA = rftest[,1]
probs_implied_test$rfcH = prCalibrate(HDA_target_train$H, as.numeric(rftrain[,3]),
                                       HDA_target_test$H, rftest[,3], nbins = 30)$cal.probs
probs_implied_test$rfcD = prCalibrate(HDA_target_train$D, as.numeric(rftrain[,2]),
                                       HDA_target_test$D, rftest[,2], nbins = 30)$cal.probs
probs_implied_test$rfcA = prCalibrate(HDA_target_train$A, as.numeric(rftrain[,1]),
                                       HDA_target_test$A, rftest[,1], nbins = 30)$cal.probs

probs_implied_test$rfcH = probs_implied_test$rfcH / (probs_implied_test$rfcH+probs_implied_test$rfcD+probs_implied_test$rfcA)
probs_implied_test$rfcD = probs_implied_test$rfcD / (probs_implied_test$rfcH+probs_implied_test$rfcD+probs_implied_test$rfcA)
probs_implied_test$rfcA = probs_implied_test$rfcA / (probs_implied_test$rfcH+probs_implied_test$rfcD+probs_implied_test$rfcA)




# MultiLayer Perceptron

require(tensorflow)
require(keras)
inputs <- layer_input(shape = list(dim(train_all_data)[2]))
reg = regularizer_l2(0.001)
hidden_layer = keras$layers$Dense(10, activation = "tanh", kernel_regularizer=reg, bias_regularizer=reg)
output_layer = keras$layers$Dense(2, activation = "softmax", kernel_regularizer=reg, bias_regularizer=reg)
outputs = output_layer(hidden_layer(inputs))
nn <- keras_model(inputs, outputs)

nn %>% compile(optimizer = "sgd", 
               loss = "categorical_crossentropy")
print(nn)
es = EarlyStopping = keras$callbacks$EarlyStopping(
  monitor="val_loss",
  min_delta=0,
  patience=50,
  verbose=0,
  mode="auto",
  restore_best_weights=TRUE)

history <- nn %>% keras::fit(as.matrix(train_all_data), 
                  as.matrix(data.frame(as.numeric(train_all_targets$GT>2.5), 1-as.numeric(train_all_targets$GT>2.5))),
                  validation_data = list(as.matrix(valid_all_data),
                                         as.matrix(data.frame(as.numeric(valid_all_targets$GT>2.5), 1-as.numeric(valid_all_targets$GT>2.5)))),
                  callbacks = list(es),
                  epochs = 200, 
                  batch_size = 64)


plot(history) + scale_y_continuous(limits = c(0.688, 0.70)) + scale_x_continuous(limits = c(0,100)) + scale_color_manual(values =  c("black", "azure4")) + scale_fill_manual(values =  c("black", "azure4")) + theme_bw()

probs_train$nn = (nn %>% predict(as.matrix(train_all_data)))[,1]
probs_valid$nn = (nn %>% predict(as.matrix(valid_all_data)))[,1]
probs_test$nn = (nn %>% predict(as.matrix(test_all_data)))[,1]
 
# MULTICLASS

inputs <- layer_input(shape = list(dim(train_all_data)[2]))
reg = regularizer_l2(0.00065)
hidden_layer = keras$layers$Dense(10, activation = "tanh", kernel_regularizer=reg, bias_regularizer=reg)
output_layer = keras$layers$Dense(3, activation = "softmax", kernel_regularizer=reg, bias_regularizer=reg)
outputs = output_layer(hidden_layer(inputs))
nn <- keras_model(inputs, outputs)

nn %>% compile(optimizer = "sgd", 
               loss = "categorical_crossentropy")
print(nn)
es = EarlyStopping = keras$callbacks$EarlyStopping(
  monitor="val_loss",
  min_delta=0,
  patience=100,
  verbose=0,
  mode="auto",
  restore_best_weights=TRUE)

history2 = nn %>% keras::fit(as.matrix(train_all_data), as.matrix(HDA_target_train),
                  validation_data = list(as.matrix(valid_all_data), as.matrix(HDA_target_valid)),
                  callbacks = list(es),
                  epochs = 300, 
                  batch_size = 64)

plot(history2) + scale_y_continuous(limits = c(1.034, 1.05)) + scale_x_continuous(limits = c(0,120)) + scale_color_manual(values =  c("black", "azure4")) + scale_fill_manual(values =  c("black", "azure4")) + theme_bw()


nntrain = nn %>% predict(as.matrix(train_all_data))
probs_implied_train$Hnn = nntrain[,1]
probs_implied_train$Dnn = nntrain[,2]
probs_implied_train$Ann = nntrain[,3]

nnvalid = nn %>% predict(as.matrix(valid_all_data))
probs_implied_valid$Hnn = nnvalid[,1]
probs_implied_valid$Dnn = nnvalid[,2]
probs_implied_valid$Ann = nnvalid[,3]

nntest = nn %>% predict(as.matrix(test_all_data))
probs_implied_test$Hnn = nntest[,1]
probs_implied_test$Dnn = nntest[,2]
probs_implied_test$Ann = nntest[,3]


# SVM ERROR MEMORY LIMIT

# Support Vector Machine
# library(e1071)
# 
# classifier = svm(formula = as.factor(`train_all_targets$GT` > 2.5) ~ .,
#                  data = cbind(train_all_data, train_all_targets$GT),
#                  type = 'C-classification',
#                  kernel = 'linear',
#                  probability=TRUE)
# 
# 
# svmpv = predict(classifier, valid_all_data, probability=TRUE)
# svmpt = predict(classifier, train_all_data, probability=TRUE)
# valid_all_targets$pred25svm = attributes(svmpv)$probabilities[,2]
# train_all_targets$pred25svm = attributes(svmpt)$probabilities[,2]
# ggplot(valid_all_targets, aes(x=pred25svm, y=Mx25pstar, color=as.factor(GT>2.5), alpha=0)) + geom_point()
# 
# brier_svm25pred = verify(thresholds = seq(0,1,0.04), obs=M25target_valid, pred=M25target_valid)
# summary(brier_svm25pred)
# 
# reliability.plot(x=brier_svm25pred$y.i, obar.i = as.matrix(data.frame(brier_Mx25pstar$obar.i, brier_svm25pred$obar.i)), prob.y = as.matrix(data.frame(brier_Mx25pstar$prob.y, brier_svm25pred$prob.y)))
# 
# ggplot(valid_all_targets, aes(x = pred25svm-Mx25pstar, fill=as.factor(GT>2.5))) + geom_histogram(position="fill")


# Reliability diagrams

r = reliabilitydiag(
  probs_valid[c("lr", "rfc", "nn")],
  y = as.integer(valid_all_targets$GT > 2.5),
  region.level = NA
)
autoplot(r) + xlim(0.25,0.75) + ylim(0.25,0.75) + scale_color_manual(values =  c("lr"="red","rfc"= "blue", "nn" = "green")) + geom_abline(slope = 1, intercept = 0, linetype = "dotted")

# Evaluation 

prob_eval_single(M25target_valid, probs_valid)
prob_eval_single(M25target_valid, probs_valid[c(-2,-3, -5)], plot=T)
View(prob_eval_multiple(HDA_target_valid, probs_implied_valid))





plot_pr = function(target, pr1, pr2){
  nums = c()
  ratios = pr1/pr2
  prs = c()
  sds = c()
  for (lambda in seq(1, 1.7, 0.01)){
    prf1 = pr1[ratios > lambda]
    prf2 = pr2[ratios > lambda]
    tarf = target[ratios > lambda]
    this_pr = mean((-1 + as.integer(tarf==1)/prf2))
    this_sd = sd((-1 + as.integer(tarf==1)/prf2))
    prs = append(prs, this_pr)
    sds = append(sds, this_sd)
    nums = append(nums, length(tarf))
  }
  prsdf = data.frame(x = seq(1, 1.7, 0.01), y = prs, s = sds, n = nums)
  return(prsdf)
}


draw_pr_plot = function(prsdf){
  colnames(prsdf) <- c("x", "y1", "s1", "n1", "y2", "s2", "n2", "y3", "s3", "n3")
  plot = ggplot(prsdf, aes(x, y1, y2, y3)) + geom_ribbon(aes(ymin = y1-s1/sqrt(n1), ymax = y1+s1/sqrt(n1), fill="GLM"), alpha=0.1) + geom_line( aes(y = y1, colour="GLM")) + 
    theme_bw() + scale_y_continuous(name = "Average Profit", limits = c(-0.3,0.3)) + 
    scale_x_continuous(name = "ratio", limits = c(1,1.4)) + geom_hline(yintercept = 0, linetype="dotted") + 
    geom_line(aes(y = y2, colour="RF")) + geom_ribbon(aes(ymin = y2-s2/sqrt(n2), ymax = y2+s2/sqrt(n2), fill="RF"), alpha=0.1)+
    geom_line(aes(y = y3, colour="NN")) + geom_ribbon(aes(ymin = y3-s3/sqrt(n3), ymax = y3+s3/sqrt(n3), fill="NN"), alpha=0.1)+ scale_fill_manual() +
    scale_colour_manual(name = "Models", values = c("GLM" ="#f04546","RF"="#3591d1","NN"="#62c76b"))+ scale_fill_manual(name = "Models", values = c("GLM" ="#f04546","RF"="#3591d1","NN"="#62c76b"))
  
  return(plot)
}

require(plyr)

# Startegy in Validation Data


prsdf1 = plot_pr(M25target_valid, probs_valid$lr, probs_valid$BbMx)
prsdf2 = plot_pr(M25target_valid, probs_valid$rfc, probs_valid$BbMx)
prsdf3 = plot_pr(M25target_valid, probs_valid$nn, probs_valid$BbMx)
prsdf = join_all(list(prsdf1, prsdf2, prsdf3), by = "x")
plot1 = (draw_pr_plot(prsdf))

prsdf1 = plot_pr(HDA_target_valid$H, probs_implied_valid$mnH, probs_implied_valid$HpstarB)
prsdf2 = plot_pr(HDA_target_valid$H, probs_implied_valid$rfcH, probs_implied_valid$HpstarB)
prsdf3 = plot_pr(HDA_target_valid$H, probs_implied_valid$Hnn, probs_implied_valid$HpstarB)
prsdf = join_all(list(prsdf1, prsdf2, prsdf3), by = "x")
plot2 = (draw_pr_plot(prsdf))


prsdf1 = plot_pr(HDA_target_valid$D, probs_implied_valid$mnD, probs_implied_valid$DpstarB)
prsdf2 = plot_pr(HDA_target_valid$D, probs_implied_valid$rfcD, probs_implied_valid$DpstarB)
prsdf3 = plot_pr(HDA_target_valid$D, probs_implied_valid$Dnn, probs_implied_valid$DpstarB)
prsdf = join_all(list(prsdf1, prsdf2, prsdf3), by = "x")
plot3 = (draw_pr_plot(prsdf))



prsdf1 = plot_pr(HDA_target_valid$A, probs_implied_valid$mnA, probs_implied_valid$ApstarB)
prsdf2 = plot_pr(HDA_target_valid$A, probs_implied_valid$rfcA, probs_implied_valid$ApstarB)
prsdf3 = plot_pr(HDA_target_valid$A, probs_implied_valid$Ann, probs_implied_valid$ApstarB)
prsdf = join_all(list(prsdf1, prsdf2, prsdf3), by = "x")
plot4 = (draw_pr_plot(prsdf))

require(ggpubr)
ggarrange(plot1, plot2, plot4, plot3, 
          labels = c(" Goals>2.5 ", "    Home", "    Away", "    Draw"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend="right")




# Startegy in Test Data
prsdf1 = plot_pr(M25target_test, probs_test$lr, probs_test$BbMx)
prsdf2 = plot_pr(M25target_test, probs_test$rfc, probs_test$BbMx)
prsdf3 = plot_pr(M25target_test, probs_test$nn, probs_test$BbMx)
prsdf = join_all(list(prsdf1, prsdf2, prsdf3), by = "x")
plot5 = (draw_pr_plot(prsdf))



prsdf1 = plot_pr(HDA_target_test$H, probs_implied_test$mnH, probs_implied_test$HpstarB)
prsdf2 = plot_pr(HDA_target_test$H, probs_implied_test$rfcH, probs_implied_test$HpstarB)
prsdf3 = plot_pr(HDA_target_test$H, probs_implied_test$Hnn, probs_implied_test$HpstarB)
prsdf = join_all(list(prsdf1, prsdf2, prsdf3), by = "x")
plot6 = (draw_pr_plot(prsdf))


prsdf1 = plot_pr(HDA_target_test$D, probs_implied_test$mnD, probs_implied_test$DpstarB)
prsdf2 = plot_pr(HDA_target_test$D, probs_implied_test$rfcD, probs_implied_test$DpstarB)
prsdf3 = plot_pr(HDA_target_test$D, probs_implied_test$Dnn, probs_implied_test$DpstarB)
prsdf = join_all(list(prsdf1, prsdf2, prsdf3), by = "x")
plot7 = (draw_pr_plot(prsdf))



prsdf1 = plot_pr(HDA_target_test$A, probs_implied_test$mnA, probs_implied_test$ApstarB)
prsdf2 = plot_pr(HDA_target_test$A, probs_implied_test$rfcA, probs_implied_test$ApstarB)
prsdf3 = plot_pr(HDA_target_test$A, probs_implied_test$Ann, probs_implied_test$ApstarB)
prsdf = join_all(list(prsdf1, prsdf2, prsdf3), by = "x")
plot8 = (draw_pr_plot(prsdf))


ggarrange(plot5, plot6, plot8, plot7, 
          labels = c(" Goals>2.5 ", "    Home", "    Away", "    Draw"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend="right")



tidx = probs_test$nn/probs_test$rfc > 1.05
ttf = test_all_targets$GT[tidx]
length(ttf)
mean((1/probs_test$rfc[tidx])*(ttf>2.5) -1)

tidx = probs_test$rfc/probs_test$nn > 1.05
ttf = test_all_targets$GT[tidx]
length(ttf)
mean((1/probs_test$nn[tidx])*(ttf>2.5) -1)

tidx = probs_test$nn/probs_test$rfc > 1.05
ttf = test_all_targets$GT[tidx]
length(ttf)
mean((1/probs_test$rfc[tidx])*(ttf>2.5) -1)


################################################################################
############## Scoreline
################################################################################




train_all_targets %>% mutate(GH = (GT+GD)/2, GA = (GT-GD)/2) -> train_all_targets
valid_all_targets %>% mutate(GH = (GT+GD)/2, GA = (GT-GD)/2) -> valid_all_targets
test_all_targets %>% mutate(GH = (GT+GD)/2, GA = (GT-GD)/2) -> test_all_targets

train_all_targets %>% group_by(GH, GA) %>% dplyr::summarise(y = n()) -> train_scorelines
valid_all_targets %>% group_by(GH, GA) %>% dplyr::summarise(y = n()) -> valid_scorelines
test_all_targets %>% group_by(GH, GA) %>% dplyr::summarise(y = n()) -> test_scorelines
train_scorelines$yr = train_scorelines$y / sum(train_scorelines$y) 
valid_scorelines$yr = valid_scorelines$y / sum(valid_scorelines$y) 
test_scorelines$yr = test_scorelines$y / sum(test_scorelines$y) 

library(scales)

ggplot(train_scorelines, aes(GH,GA)) +
  geom_tile(aes(fill = yr), colour = "white") + xlim(-0.6,5.6) + ylim(-0.6, 5.6) + 
  geom_text(aes(label = percent(yr,0.01)), vjust = 1, colour = "black") +
  scale_fill_gradient(low = "white", high = "azure4") +
  theme_bw() + theme(legend.position = "none")

prob_eval_scoreline = function(probs, plot = 0, rho=0, poisson=F){
  target = probs[,1:2]
  probs = probs[,3:dim(probs)[2]]
  LikelihoodsH = list()
  LikelihoodsA = list()
  Likelihoods = list()
  plots = list()
  for (i in seq(1:((length(probs)/2)))){
    this_probs = probs[,(2*i-1):(2*i)]
    if (poisson){
        llh = dpois(target[,1], this_probs[,1])
        lla = dpois(target[,2], this_probs[,2])
        llt = llh * lla
        llt = llt*(1-rho)
        llt = llt + rho*(as.integer(target[,1]==target[,2]))*dpois(target[,1], poisson)
        llt = -log(llt)
        llh = -log(llh)
        lla = -log(lla)
     } else {
       llh = -dpois(target[,1], this_probs[,1], log=T)
       lla = -dpois(target[,2], this_probs[,2], log=T)
       llt = llh + lla
       if (rho){
         ltau = tau(target[,1],target[,2], this_probs[,1], this_probs[,2], rho = rho)
         llt = llt - log(ltau)
       } 
     }
    
  
  
    if (plot){
      hist(llt)
      this_scoreline = data.frame()
      for (gghh in c(0,1,2,3,4,5,6,7,8,9)){
        for (ggaa in c(0,1,2,3,4,5,6,7,8,9)){
          this_prob = dpois(gghh, this_probs[,1])*dpois(ggaa, this_probs[,2])
          if (poisson){
            this_prob = (1-rho)*this_prob
            if(gghh == ggaa){
              this_prob = this_prob + rho*dpois(gghh, poisson)
            }
          } else if (ggaa < 2 & gghh < 2 & rho){
            addtau = tau(rep(gghh, dim(target)[1]), rep(ggaa, dim(target)[1]), this_probs[,1], this_probs[,2], rho=rho)
            this_prob = addtau*this_prob
          }
          this_prob = sum(this_prob)
          this_scoreline <- rbind(this_scoreline, data.frame(GH = gghh, GA = ggaa, y = as.integer(this_prob)))
        }
      }
      this_scoreline$yr = this_scoreline$y / sum(this_scoreline$y) 
      merge(this_scoreline, train_scorelines, by=c("GH","GA")) %>% mutate (diff = yr.x - yr.y) -> this_scoreline
      
      plots[[i*3-2]] = ggplot(this_scoreline, aes(GH,GA)) +
        geom_tile(aes(fill = yr.x), colour = "white") + xlim(-0.6,5.6) + ylim(-0.6, 5.6) + 
        geom_text(aes(label = percent(yr.x, 0.01)), vjust = 1, colour = "black") +
        scale_fill_gradient(low = "white", high = "azure4") +
        theme_bw() + theme(legend.position = "none")
        
      plots[[i*3-1]] = ggplot(this_scoreline, aes(GH,GA)) +
        geom_tile(aes(fill = diff), colour = "white") + xlim(-0.6,5.6) + ylim(-0.6, 5.6) + 
        geom_text(aes(label = percent(diff, 0.01)), vjust = 1, colour = "black") +
        scale_fill_gradient(low = "white", high = "azure4") +
        theme_bw() + theme(legend.position = "none")
      plots[[i*3]] = ggplot(data = data.frame("TH" = target[,1]+target[,2], "PH" = this_probs[,1]+this_probs[,2]), aes(as.factor(TH), PH)) + geom_violin()
      
    }
    LikelihoodsH[[i]] = mean(llh)
    LikelihoodsA[[i]] = mean(lla)
    Likelihoods[[i]] = mean(llt)
    
    # LLs[i] = cross.entropy(HDA_target_train, this_probs)
    # RPSs[i] = rps(obs=as.matrix(as.integer(target_classes)), pred=as.matrix(this_probs))$rps
  }
  return(list("Likelihoods" = Likelihoods,
              "LLH" = LikelihoodsH,
              "LLA" = LikelihoodsA,
              "Plots" = plots))
  # return(list("LLs"=LLs, "RPSs"=RPSs))
}


probs_scoreline_train = data.frame("GH" = train_all_targets$GH, "GA" = train_all_targets$GA)
probs_scoreline_valid = data.frame("GH" = valid_all_targets$GH, "GA" = valid_all_targets$GA)
probs_scoreline_test = data.frame("GH" = test_all_targets$GH, "GA" = test_all_targets$GA)

# Double Poisson GLM

scoreline_lrh = glm(`train_all_targets$GH` ~ ., data = cbind(train_all_data, train_all_targets$GH), family = "poisson")
scoreline_lra = glm(`train_all_targets$GA` ~ ., data = cbind(train_all_data, train_all_targets$GA), family = "poisson")

probs_scoreline_train$GLMFH = predict(scoreline_lrh, train_all_data, type = "response")
probs_scoreline_train$GLMFA = predict(scoreline_lra, train_all_data, type = "response")

probs_scoreline_valid$GLMFH = predict(scoreline_lrh, valid_all_data, type = "response")
probs_scoreline_valid$GLMFA = predict(scoreline_lra, valid_all_data, type = "response")

probs_scoreline_test$GLMFH = predict(scoreline_lrh, test_all_data, type = "response")
probs_scoreline_test$GLMFA = predict(scoreline_lra, test_all_data, type = "response")

prob_eval_scoreline(probs_scoreline_train)



# INFLATIONS

tau = function(x,y,l,m, rho, poisson=F){
  if (poisson){
    all_r = c()
    for (i in seq(1:length(x))){
      if (x[i] == y[i]){
        all_r = append(all_r,dpois(x[i], rho))
      }  
      if (x[i] != y[i]){
        all_r = append(all_r,0)
      }
    }
    return(all_r)
  }
  
  all_r = c()
  for (i in seq(1:length(x))){
    if (x[i] == 0 & y[i] == 0){
      all_r = append(all_r,1-l[i]*m[i]*rho)
    }  
    if (x[i] == 0 & y[i] == 1){
      all_r = append(all_r,1+l[i]*rho)
    }  
    if (x[i] == 1 & y[i] == 0){
      all_r = append(all_r,1+m[i]*rho)
    }  
    if (x[i] == 1 & y[i] == 1){
      all_r = append(all_r,1-rho)
    }
    if (x[i] > 1 | y[i] > 1){
      all_r = append(all_r,1)
    }
  }
  return(all_r)
}

find_poisson = function(x,y,l,m,plot=0){
  hps = expand.grid((seq(0.005,0.02,0.00025)), (seq(0.4, 1.4, 0.02)))
  colnames(hps) = c("p", "theta")
  for (i in seq(1:dim(hps)[1])){
    llh = dpois(x, l)
    lla = dpois(y, m)
    llt = llh * lla
    llt = llt*(1-hps$p[i])
    llt = llt + hps$p[i]*(as.integer(x==y))*dpois(x, as.double(hps$theta[i]))
    llt = -log(llt)
    llt = mean(llt)
    hps$loss[i] = llt
  }
  
  if (plot){
    plot = ggplot(hps, aes(p, theta, fill= loss)) + 
      geom_tile() + scale_x_continuous(breaks = seq(0,0.1,0.01)) + scale_y_continuous(breaks = seq(0,0.1,0.01)) + scale_fill_gradient(low="black", high="white") + theme_bw()
    plot
  }
  p_best = hps$p[hps$loss == min(hps$loss)]
  theta_best = hps$theta[hps$loss == min(hps$loss)]
  return(list(p_best, theta_best, plot))
}

find_rho = function(x,y,l,m,plot=0){
  all_lls = c()
  seq_grid = seq(-0.04, -0.02, 0.001)
  for (rho in seq_grid){
    lt = log(tau(x, y, l, m, rho=rho)) 
    all_lls=append(all_lls, mean(-lt))
  }
  if (plot){
    plot(seq_grid, all_lls, type="l", xlab="Rho", ylab= "Loss")
  }
  rho_best = seq_grid[all_lls == min(all_lls)]
  return(rho_best)
}

rho_best = find_rho(train_all_targets$GH, train_all_targets$GA, probs_scoreline_train$GLMFH, probs_scoreline_train$GLMFA, plot=T)
print(rho_best)



pt_best = find_poisson(train_all_targets$GH, train_all_targets$GA, probs_scoreline_train$GLMFH, probs_scoreline_train$GLMFA, plot=T)
print(pt_best)

# BIVARIATE POISSONS

library(bivpois)
#l3=0 should be equal to glm case
scoreline_bpz <-lm.bp(GH~. , GA~. , zeroL3=T , data=cbind(train_all_data, data.frame(GH = train_all_targets$GH, GA = train_all_targets$GA)), maxit = 5)	
bpz_coeff_h = c()
bpz_coeff_a = c()
for (t in names(scoreline_lrh$coefficients)){
  bpz_coeff_h = append(bpz_coeff_h, scoreline_bpz$coefficients[paste("(l1):", t, sep="")])
  bpz_coeff_a = append(bpz_coeff_a, scoreline_bpz$coefficients[paste("(l2):", t, sep="")])
}
print(mean(bpz_coeff_h - scoreline_lrh$coefficients))
print(mean(bpz_coeff_a - scoreline_lra$coefficients))

# Inflation
scoreline_bpzg <-lm.dibp(GH~. , GA~. , zeroL3 = T, data=cbind(train_all_data, data.frame(GH = train_all_targets$GH, GA = train_all_targets$GA)), maxit = 50, distribution="geometric")	
print(-scoreline_bpzg$loglikelihood[51]/dim(train_all_data)[1]) #Loss
scoreline_bpzp <-lm.dibp(GH~. , GA~. , zeroL3 = T, data=cbind(train_all_data, data.frame(GH = train_all_targets$GH, GA = train_all_targets$GA)), maxit = 50, distribution="poisson")	
print(-scoreline_bpzp$loglikelihood[51]/dim(train_all_data)[1]) #Loss

# l3 > 0 
scoreline_bpc <-lm.bp(GH~. , GA~. , l3 = ~1, data=cbind(train_all_data, data.frame(GH = train_all_targets$GH, GA = train_all_targets$GA)), maxit = 50)	
print(-scoreline_bpc$loglikelihood[51]/dim(train_all_data)[1]) #Loss
print(exp(scoreline_bpc$coefficients["(l3):(Intercept)"]))
## Inflation
scoreline_bpci <-lm.dibp(GH~. , GA~. , l3 = ~1, data=cbind(train_all_data, data.frame(GH = train_all_targets$GH, GA = train_all_targets$GA)), maxit = 50, distribution="poisson")	
print(-scoreline_bpci$loglikelihood[51]/dim(train_all_data)[1]) #Loss
print(exp(scoreline_bpci$coefficients["(l3):(Intercept)"]))


# l3 = full
scoreline_bpf <-lm.bp(GH~. , GA~. , l3 = ~., data=cbind(train_all_data, data.frame(GH = train_all_targets$GH, GA = train_all_targets$GA)), maxit = 100)	
print(-scoreline_bpf$loglikelihood[51]/dim(train_all_data)[1]) #Loss
plot(scoreline_bpf$coefficients[52:75], type="l", col="red")
probs_scoreline_train$BPFH = scoreline_bpf$lambda1 + scoreline_bpf$lambda3 
probs_scoreline_train$BPFA = scoreline_bpf$lambda2 + scoreline_bpf$lambda3 


## Inflation
scoreline_bpfi <-lm.dibp(GH~. , GA~. , l3 = ~., data=cbind(train_all_data, data.frame(GH = train_all_targets$GH, GA = train_all_targets$GA)), maxit = 100, distribution="poisson")	
print(-scoreline_bpfi$loglikelihood[101]/dim(train_all_data)[1]) #Loss
lines(scoreline_bpfi$coefficients[52:75], type="l", col="blue")
probs_scoreline_train$BPFIH = scoreline_bpfi$lambda1 + scoreline_bpfi$lambda3 
probs_scoreline_train$BPFIA = scoreline_bpfi$lambda2 + scoreline_bpfi$lambda3 

p_best = scoreline_bpfi$coefficients["p"]
theta_best = scoreline_bpfi$coefficients["theta"]

# Compare Bivariate poissons and glm double poissons

bpz_coeff_h = c()
bpz_coeff_a = c()
bpz_coeff_c = c()
for (t in names(scoreline_lrh$coefficients)){
  bpz_coeff_h = append(bpz_coeff_h, scoreline_bpf$coefficients[paste("(l1):", t, sep="")])
  bpz_coeff_a = append(bpz_coeff_a, scoreline_bpf$coefficients[paste("(l2):", t, sep="")])
  bpz_coeff_c = append(bpz_coeff_c, scoreline_bpf$coefficients[paste("(l3):", t, sep="")])
}

vl1 = exp(bpz_coeff_h[1] + as.matrix(valid_all_data) %*% as.matrix(bpz_coeff_h[2:25]))
vl2 = exp(bpz_coeff_a[1] + as.matrix(valid_all_data) %*% as.matrix(bpz_coeff_a[2:25]))
vl3 = exp(bpz_coeff_c[1] + as.matrix(valid_all_data) %*% as.matrix(bpz_coeff_c[2:25]))
probs_scoreline_valid$BPFH = vl1 + vl3 
probs_scoreline_valid$BPFA = vl2 + vl3

vl1 = exp(bpz_coeff_h[1] + as.matrix(test_all_data) %*% as.matrix(bpz_coeff_h[2:25]))
vl2 = exp(bpz_coeff_a[1] + as.matrix(test_all_data) %*% as.matrix(bpz_coeff_a[2:25]))
vl3 = exp(bpz_coeff_c[1] + as.matrix(test_all_data) %*% as.matrix(bpz_coeff_c[2:25]))
probs_scoreline_test$BPFH = vl1 + vl3 
probs_scoreline_test$BPFA = vl2 + vl3


rho_best_full = find_rho(train_all_targets$GH, train_all_targets$GA, scoreline_bpf$fitted.values[,1], scoreline_bpf$fitted.values[,2], plot=T)
print(rho_best_full - rho_best)


tmp = data.frame(Feature = names(scoreline_lrh$coefficients[2:25]), Model=c(rep("Double", 24), rep("Bivariate", 24)), Val = append(scoreline_lrh$coefficients[2:25], bpz_coeff_h[2:25]))
tmp$Feature = factor(tmp$Feature, levels = tmp$Feature[1:24])
plot1 = ggplot(data = tmp, aes(x= Feature, y=Val, fill=Model)) + geom_bar(position="dodge", stat="identity") + theme_bw() + scale_y_continuous(name = "Coefficient") + scale_fill_manual(values = c("Double"= "azure4","Bivariate"= "darkgray"))

tmp = data.frame(Feature = names(scoreline_lra$coefficients[2:25]), Model=c(rep("Double", 24), rep("Bivariate", 24)), Val = append(scoreline_lra$coefficients[2:25], bpz_coeff_a[2:25]))
tmp$Feature = factor(tmp$Feature, levels = tmp$Feature[1:24])
plot2 = ggplot(data = tmp, aes(x= Feature, y=Val, fill=Model)) + geom_bar(position="dodge", stat="identity") + theme_bw() + scale_y_continuous(name = "Coefficient") + scale_fill_manual(values = c("Double"= "azure4","Bivariate"= "darkgray"))

ggarrange(plot1, plot2, 
          labels = c(" Home Goals", "  Away Goals"),
          ncol = 1, nrow = 2, common.legend = TRUE, legend="right")


# Teams General and Attack-Defense Ratings
tmp = data.frame(Feature = rep(names(scoreline_lrh$coefficients[2:13]), 2), Model=c(rep("GammaH", 12), rep("GammaA", 12)), Val = append(bpz_coeff_h[2:13] - bpz_coeff_a[2:13], bpz_coeff_a[14:25] - bpz_coeff_h[14:25] ))
tmp$Feature = factor(tmp$Feature, levels = tmp$Feature[1:12])
ggplot(data = tmp, aes(x= Feature, y=Val, fill=Model)) + geom_bar(position="dodge", stat="identity") + theme_bw() + scale_y_continuous(name = "Coefficient") + scale_fill_manual(values = c("GammaH"= "azure4","GammaA"= "darkgray"))

tmp = data.frame(Feature = rep(names(scoreline_lrh$coefficients[2:13]), 2), Model=c(rep("GammaH", 12), rep("GammaA", 12)), Val = append(bpz_coeff_h[2:13], bpz_coeff_a[14:25] ))
tmp$Feature = factor(tmp$Feature, levels = tmp$Feature[1:12])
ggplot(data = tmp, aes(x= Feature, y=Val, fill=Model)) + geom_bar(position="dodge", stat="identity") + theme_bw() + scale_y_continuous(name = "Coefficient") + scale_fill_manual(values = c("GammaH"= "azure4","GammaA"= "darkgray"))

tmp = data.frame(Feature = rep(names(scoreline_lrh$coefficients[2:13]), 2), Model=c(rep("GammaH", 12), rep("GammaA", 12)), Val = append(bpz_coeff_h[14:25], bpz_coeff_a[2:13] ))
tmp$Feature = factor(tmp$Feature, levels = tmp$Feature[1:12])
ggplot(data = tmp, aes(x= Feature, y=Val, fill=Model)) + geom_bar(position="dodge", stat="identity") + theme_bw() + scale_y_continuous(name = "Coefficient") + scale_fill_manual(values = c("GammaH"= "azure4","GammaA"= "darkgray"))

# Neural Networks Initializing
library(tensorflow)
library(keras)
feature_size = 24
features <- layer_input(shape = list(feature_size))
features_home = features[,1:12]
features_away = features[,13:24]
losses = list(keras$losses$Poisson(name = "LossHome") , keras$losses$Poisson(name = "LossAway"))
XT = as.matrix(train_all_data)
YT = list(as.matrix(data.frame(train_all_targets$GH)), as.matrix(data.frame(train_all_targets$GA)))
XV = as.matrix(valid_all_data)
YV = list(as.matrix(data.frame(valid_all_targets$GH)), as.matrix(data.frame(valid_all_targets$GA)))

# team general ratings

rank_general = keras$layers$Dense(1)
home_rank = rank_general(features[,1:12])
away_rank = rank_general(features[,13:24])
dif = home_rank - away_rank
home_effect = as.double(scoreline_lrh$coefficients[1] - scoreline_lra$coefficients[1])
print(c("Home Effect: ", home_effect))
lambda_home_dif_rank_dense = keras$layers$Dense(1, activation="exponential", kernel_initializer = initializer_constant(1))
lambda_home_dif_rank = lambda_home_dif_rank_dense(dif)
lambda_away_dif_rank_dense = keras$layers$Dense(1, activation="exponential", kernel_initializer = initializer_constant(1), use_bias = F)
lambda_away_dif_rank = lambda_away_dif_rank_dense(-dif)
lambda_c_dense = keras$layers$Dense(1, activation="exponential")
lambda_c = lambda_c_dense(features)[,1]

goals_home_dif_rank = lambda_home_dif_rank + lambda_c
goals_away_dif_rank = lambda_away_dif_rank + lambda_c

nnrating_g <- keras_model(features, list(goals_home_dif_rank, goals_away_dif_rank))

nnrating_g %>% compile(optimizer='adam', loss=losses, loss_weights=list(1,1)) -> nnrating_g
print(nnrating_g)
nnrating_g %>% keras::fit(XT, YT,
                        validation_data = list(XV, YV),
                        epochs = 100, 
                        batch_size = 128)

nnrating_g$predict(XT, batch_size=as.integer(8)) -> tra_predsnn
probs_scoreline_train$NNGRH = tra_predsnn[[1]] 
probs_scoreline_train$NNGRA = tra_predsnn[[2]]

nnrating_g$predict(as.matrix(valid_all_data), batch_size=as.integer(3)) -> tra_predsnn
probs_scoreline_valid$NNGRH = tra_predsnn[[1]] 
probs_scoreline_valid$NNGRA = tra_predsnn[[2]]

nnrating_g$predict(as.matrix(test_all_data), batch_size=as.integer(2)) -> tra_predsnn
probs_scoreline_test$NNGRH = tra_predsnn[[1]]
probs_scoreline_test$NNGRA = tra_predsnn[[2]]

# RHO is always the same
rho_best_nngr = find_rho(train_all_targets$GH, train_all_targets$GA, probs_scoreline_train$NNGRH, probs_scoreline_train$NNGRA, plot=T)
print(rho_best_nngr - rho_best)

# Plot Coefficients

scale1 = function(x) {return((x-mean(x))/sd(x))}
tmp = data.frame(Feature = rep(names(scoreline_lrh$coefficients[2:13]), 3), Model=c(rep("GammaH", 12), rep("GammaA", 12), rep("Gamma", 12)), Val = append(scale1(bpz_coeff_h[2:13] - bpz_coeff_a[2:13]), append(scale1(bpz_coeff_a[14:25] - bpz_coeff_h[14:25]),scale1(as.array(rank_general$weights[[1]])[,1]
) )))
tmp$Feature = factor(tmp$Feature, levels = tmp$Feature[1:12])
ggplot(data = tmp, aes(x= Feature, y=Val, fill=Model)) + geom_bar(position="dodge", stat="identity") + theme_bw() + scale_y_continuous(name = "Coefficient") + scale_fill_manual(values = c("Gamma"="gray","GammaA"= "darkgray", "GammaH"= "azure4"))


# attack and defence

rank_attack_defense = keras$layers$Dense(2, activation = "exponential")
home_params = rank_attack_defense(features[,1:12])
away_params = rank_attack_defense(features[,13:24])

home_effect = keras$layers$Dense(1, use_bias = F, kernel_initializer = initializer_constant(1))
home_goals_attack_defense = keras$layers$Reshape(target_shape=list(as.integer(1)))(home_params[,1]*away_params[,2])
home_goals_attack_defense = home_effect(home_goals_attack_defense)[,1]
# hg = home_atts[,1]*away_atts[,2]
away_goals_attack_defense = home_params[,2]*away_params[,1]
nnrating_ad <- keras_model(features, list(home_goals_attack_defense, away_goals_attack_defense))
nnrating_ad %>% compile(optimizer='adam', loss=losses, loss_weights=list(1,1)) -> nnrating_ad
print(nnrating_ad)
nnrating_ad %>% keras::fit(XT, YT,
                           validation_data = list(XV, YV),
                           epochs = 100, 
                           batch_size = 128)


nnrating_ad %>% predict(XT, type="response") -> tra_predsnn
probs_scoreline_train$NNADH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_train$NNADA = as.numeric(tra_predsnn[[2]])

nnrating_ad %>% predict(as.matrix(valid_all_data), type="response") -> tra_predsnn
probs_scoreline_valid$NNADH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_valid$NNADA = as.numeric(tra_predsnn[[2]])

nnrating_ad %>% predict(as.matrix(test_all_data), type="response") -> tra_predsnn
probs_scoreline_test$NNADH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_test$NNADA = as.numeric(tra_predsnn[[2]])

# RHO is always the same
rho_best_nnad = find_rho(train_all_targets$GH, train_all_targets$GA, probs_scoreline_train$NNADH, probs_scoreline_train$NNADA, plot=T)
print(rho_best_nnad - rho_best)


tmp = data.frame(Feature = rep(names(scoreline_lrh$coefficients[2:13]), 3), Model=c(rep("AttackH", 12), rep("AttackA", 12), rep("Attack", 12)), Val = append(scale1(bpz_coeff_h[2:13]), append(scale1(bpz_coeff_a[14:25]),scale1(as.array(rank_attack_defense$weights[[1]])[,1]
) )))
tmp$Feature = factor(tmp$Feature, levels = tmp$Feature[1:12])
plot1 = ggplot(data = tmp, aes(x= Feature, y=Val, fill=Model)) + geom_bar(position="dodge", stat="identity") + theme_bw() + scale_y_continuous(name = "Coefficient") + scale_fill_manual(values = c("Attack"="gray","AttackA"= "darkgray", "AttackH"= "azure4"))


tmp = data.frame(Feature = rep(names(scoreline_lrh$coefficients[2:13]), 3), Model=c(rep("DefenseH", 12), rep("DefenseA", 12), rep("Defense", 12)), Val = append(scale1(bpz_coeff_h[14:25]), append(scale1(bpz_coeff_a[2:13]),scale1(as.array(rank_attack_defense$weights[[1]])[,2]
) )))
tmp$Feature = factor(tmp$Feature, levels = tmp$Feature[1:12])
plot2 = ggplot(data = tmp, aes(x= Feature, y=Val, fill=Model)) + geom_bar(position="dodge", stat="identity") + theme_bw() + scale_y_continuous(name = "Coefficient") + scale_fill_manual(values = c("Defense"="gray","DefenseA"= "darkgray", "DefenseH"= "azure4"))

ggarrange(plot1, plot2, 
          labels = c("  Attack", "  Defense"),
          ncol = 1, nrow = 2)

# Clustering

library(factoextra)
n_clusters = 20
km <- kmeans(rbind(as.matrix(train_all_data[,1:12]), as.matrix(train_all_data[,13:24])), centers = n_clusters, nstart = 20)
# fviz_cluster(km, data = train_all_data, geom = "point", ellipse=T, ellipse.type = "t") + theme_bw()

cluster_ad_ranks = rank_attack_defense(km$centers)$numpy()
cluster_gr_ranks = rank_general(km$centers)$numpy()



clusters_scorelines_train = data.frame("CH" = km$cluster[1:dim(train_all_data)[1]], 
                                        "CA" = km$cluster[(dim(train_all_data)[1]+1):(2*dim(train_all_data)[1])],
                                        "GH" = train_all_targets$GH,
                                        "GA" = train_all_targets$GA)

cstd = clusters_scorelines_train[clusters_scorelines_train$CH == clusters_scorelines_train$CA,]
csto = clusters_scorelines_train[clusters_scorelines_train$CH != clusters_scorelines_train$CA,]
cstox = csto[1:5000,] #Memory Limit

print(mean(cstd$GH == cstd$GA))
print(mean(csto$GH == csto$GA))

# Make a vector of all team names. 
all_teams <- sort(unique(c(csto$CH, csto$CA)), decreasing = FALSE)
n_teams <- length(all_teams)

# list of parameters with initial values.
parameter_list <- list(attack = rep(0.2, n_teams),
                       defense = rep(-0.01, n_teams-1),
                       home = 0.1)

names(parameter_list$attack) <- all_teams
names(parameter_list$defense) <- all_teams[-1] # the first parameter is computed from the rest.
dc_negloglik <- function(params, goals_home, goals_visitor,
                         team_home, team_visitor, param_skeleton){
  
  # relist, to make things easier.
  plist <- relist(params, param_skeleton)
  
  # There is a sum-to-zero constraint on defense parameters.
  # The defense parameter for the first team is computed from the rest.
  plist$defense <- c(sum(plist$defense)*-1, plist$defense)
  names(plist$defense)[1] <- names(plist$attack[1]) # add name to first element.
  
  # Home team expected goals
  lambda_home <- exp(plist$attack[team_home] + plist$defense[team_visitor] + plist$home)
  
  # Away team expected goals
  lambda_visitor <- exp(plist$attack[team_visitor] + plist$defense[team_home])
  
  # Dixon-Coles adjustment
  dc_adj <- tau(goals_home, goals_visitor, lambda_home, lambda_visitor, rho = rho_best)
  # 
  # print(sum(is.na(team_home)))
  # print(sum(is.na(plist$attack[team_home])))
  # print(sum(is.na(plist$defense[team_visitor])))
  # print(sum(is.na(plist$home)))
  
  # Trick to avoid warnings.
  if (any(dc_adj <= 0)){
    return(Inf)
  }
  
  # The log-likelihood
  log_lik_home <- dpois(goals_home, lambda = lambda_home, log=TRUE)
  log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log=TRUE)
  
  log_lik <- sum((log_lik_home + log_lik_visitor + log(dc_adj)))
  
  return(log_lik*-1)
  
}

optim_res <- optim(par = unlist(parameter_list), fn=dc_negloglik,
                   goals_home = cstox$GH,
                   goals_visitor = cstox$GA,
                   team_home = cstox$CH, team_visitor = cstox$CA,
                   param_skeleton=parameter_list, method = 'BFGS')

# relist, and calculate the remaining parameter. 
parameter_est <- relist(optim_res$par, parameter_list)
parameter_est$defense <- c( sum(parameter_est$defense) * -1, parameter_est$defense)
names(parameter_est$defense)[1] <- names(parameter_est$attack[1]) 





tmp = data.frame(Cluster = rep(as.character(seq(1:n_clusters)), 2), Model=c(rep("Actual", n_clusters), rep("Predicted", n_clusters)), Val = append(scale1(cluster_ad_ranks[,1]), scale1(parameter_est$attack) ))
tmp$Cluster = factor(tmp$Cluster, levels = tmp$Cluster[1:n_clusters])
plot1 = ggplot(data = tmp, aes(x= Cluster, y=Val, fill=Model)) + geom_bar(position="dodge", stat="identity") + theme_bw() + scale_y_continuous(name = "Value") + scale_fill_manual(values = c("Predicted"= "darkgray", "Actual"= "azure4"))

tmp = data.frame(Cluster = rep(seq(1:n_clusters), 2), Model=c(rep("Actual", n_clusters), rep("Predicted", n_clusters)), Val = append(scale1(cluster_ad_ranks[,2]), scale1(parameter_est$defense) ))
tmp$Cluster = factor(tmp$Cluster, levels = tmp$Cluster[1:n_clusters])
plot2 = ggplot(data = tmp, aes(x= Cluster, y=Val, fill=Model)) + geom_bar(position="dodge", stat="identity") + theme_bw() + scale_y_continuous(name = "Value") + scale_fill_manual(values = c("Predicted"= "darkgray", "Actual"= "azure4"))

ggarrange(plot1, plot2, 
          labels = c("  Attack", " Defense"),
          ncol = 1, nrow = 2, common.legend = TRUE, legend="right")


# Neural Networks

reg = regularizer_l2(0.00065)
nnhidden = keras$layers$Dense(10, activation = "relu", kernel_regularizer = reg, bias_regularizer = reg)
nngoals = keras$layers$Dense(3, activation = "exponential", kernel_regularizer = reg, bias_regularizer = reg)
nnpreds = nngoals(features)
nnbp <- keras_model(features, list(nnpreds[,1]+nnpreds[,3], nnpreds[,2]+nnpreds[,3]))
nnbp %>% compile(optimizer='adam', loss=losses, loss_weights=list(1,1)) -> nnbp
print(nnbp)
es = EarlyStopping = keras$callbacks$EarlyStopping(
  monitor="val_loss",
  min_delta=0,
  patience=200,
  verbose=0,
  mode="auto",
  restore_best_weights=TRUE)
history = nnbp %>% keras::fit(XT, YT,
                    validation_data = list(XV, YV),
                    epochs = 600,
                    callbacks = list(es),
                    batch_size = 128)

plot(history) + scale_color_manual(values =  c("black", "azure4")) + scale_fill_manual(values =  c("black", "azure4")) + theme_bw() + xlim(0,300)


nnbp %>% predict(as.matrix(train_all_data), type="response") -> tra_predsnn
probs_scoreline_train$NNBPH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_train$NNBPA = as.numeric(tra_predsnn[[2]])

nnbp %>% predict(as.matrix(valid_all_data), type="response") -> tra_predsnn
probs_scoreline_valid$NNBPH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_valid$NNBPA = as.numeric(tra_predsnn[[2]])

nnbp %>% predict(as.matrix(test_all_data), type="response") -> tra_predsnn
probs_scoreline_test$NNBPH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_test$NNBPA = as.numeric(tra_predsnn[[2]])



reg = regularizer_l2(0.00065)
nnhidden = keras$layers$Dense(10, activation = "tanh", kernel_regularizer = reg, bias_regularizer = reg)
nnhidden2 = keras$layers$Dense(10, activation = "relu", kernel_regularizer = reg, bias_regularizer = reg)
# nndcmodel = keras$layers$Dense(1, activation = "exponential")
nnhatts = nnhidden(features_home)
nnaatts = nnhidden(features_away)
nngha = keras$layers$Concatenate()(c(nnhatts, nnaatts))
outgh = keras$layers$Dense(2, activation = "exponential", kernel_regularizer = reg, bias_regularizer = reg)(nngha)

nncom <- keras_model(features, list(outgh[,1], outgh[,2]))
nncom %>% compile(optimizer='adam', loss=losses, loss_weights=list(1,1)) -> nncom
print(nncom)
history = nncom %>% keras::fit(XT, YT,
                               validation_data = list(XV, YV),
                               epochs = 500,
                               callbacks = list(es),
                               batch_size = 128)

plot(history) + scale_color_manual(values =  c("black", "azure4")) + scale_fill_manual(values =  c("black", "azure4")) + theme_bw() + xlim(0,300)


nncom %>% predict(as.matrix(train_all_data), type="response") -> tra_predsnn
probs_scoreline_train$NNCCH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_train$NNCCA = as.numeric(tra_predsnn[[2]])

nncom %>% predict(as.matrix(valid_all_data), type="response") -> tra_predsnn
probs_scoreline_valid$NNCCH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_valid$NNCCA = as.numeric(tra_predsnn[[2]])

nncom %>% predict(as.matrix(test_all_data), type="response") -> tra_predsnn
probs_scoreline_test$NNCCH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_test$NNCCA = as.numeric(tra_predsnn[[2]])


reg = regularizer_l2(0.00065)
nnhidden = keras$layers$Dense(10, activation = "tanh", kernel_regularizer = reg, bias_regularizer = reg)
nnhidden2 = keras$layers$Dense(10, activation = "relu", kernel_regularizer = reg, bias_regularizer = reg)
# nndcmodel = keras$layers$Dense(1, activation = "exponential")
nnhatts = nnhidden(features_home)
nnaatts = nnhidden(features_away)
nngha = keras$layers$Concatenate()(c(nnhatts[,1:5], nnaatts[,6:10]))
nngaa = keras$layers$Concatenate()(c(nnhatts[,6:10], nnaatts[,1:5]))
outgh = keras$layers$Dense(1, activation = "exponential", kernel_regularizer = reg, bias_regularizer = reg)(nngha)
outga = keras$layers$Dense(1, activation = "exponential", kernel_regularizer = reg, bias_regularizer = reg)(nngaa)

nncom <- keras_model(features, list(outgh, outga))
nncom %>% compile(optimizer='adam', loss=losses, loss_weights=list(1,1)) -> nncom
print(nncom)
history = nncom %>% keras::fit(XT, YT,
                               validation_data = list(XV, YV),
                               epochs = 1000,
                               callbacks = list(es),
                               batch_size = 128)


plot(history) + scale_color_manual(values =  c("black", "azure4")) + scale_fill_manual(values =  c("black", "azure4")) + theme_bw() + xlim(0,300)

nncom %>% predict(as.matrix(train_all_data), type="response") -> tra_predsnn
probs_scoreline_train$NNCMH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_train$NNCMA = as.numeric(tra_predsnn[[2]])

nncom %>% predict(as.matrix(valid_all_data), type="response") -> tra_predsnn
probs_scoreline_valid$NNCMH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_valid$NNCMA = as.numeric(tra_predsnn[[2]])

nncom %>% predict(as.matrix(test_all_data), type="response") -> tra_predsnn
probs_scoreline_test$NNCMH = as.numeric(tra_predsnn[[1]]) 
probs_scoreline_test$NNCMA = as.numeric(tra_predsnn[[2]])

# Evaluation

best_poisson = find_poisson(probs_scoreline_train$GH, probs_scoreline_train$GA, probs_scoreline_train$NNCMH, probs_scoreline_train$NNCMA, plot=T)
print(best_poisson)
best_alpha = best_poisson[[1]]
best_theta = best_poisson[[2]]

r1 = prob_eval_scoreline(probs_scoreline_test, rho=0, plot = F)
r2 = prob_eval_scoreline(probs_scoreline_test, rho=rho_best, plot = F)
r3 = prob_eval_scoreline(probs_scoreline_test, rho=best_alpha, poisson=best_theta, plot = F)



# Extra PLOTS

ggpairs(train_all_data[1:6]) + theme_bw()

tmpd = data.frame(Team = append(rep("Home",30008), rep("Away",30008)), Goals = append(train_all_targets$GH, train_all_targets$GA))
ggplot(data = tmpd, aes(x=as.numeric(Goals), fill=Team),) + geom_histogram(position="dodge") + theme_bw() +xlim(-0.6,7.6)



ggplot(tmpd, aes(x=Goals, fill=Team)) +
  geom_histogram(aes(y = stat(count) / sum(count)), position="dodge", binwidth = 1)+scale_y_continuous(labels = scales::percent)+
  geom_vline(data = data.frame(Team=c("Home", "Away"), Goals=c(mean(train_all_targets$GH),mean(train_all_targets$GA))), aes(xintercept=Goals, linetype=Team))+
  theme_bw() + theme(legend.position="top") + scale_fill_manual(values=c("gray", "azure4")) + scale_linetype_manual(values=c("dashed", "dotted")) + scale_x_continuous(name ="Goals", limits=c(-0.6,6.6), breaks = seq(0,6)) + scale_y_continuous(name = "frequency")


