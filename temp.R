











gm_res <- goalmodel(goals1 = csto$GH, goals2 = csto$GA,
                    team1 = as.character(csto$CH), team2 = as.character(csto$CH))
summary(gm_res)


library(goalmodel)
library(engsoccerdata) # Some data.
library(Rcpp)

england %>% 
  filter(Season %in% c(2011),
         tier==c(1)) %>% 
  mutate(Date = as.Date(Date)) -> england_2011

# Fit the default model, with the home team as team1.

gm_res <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                    team1 = england_2011$home, team2=england_2011$visitor)

# Show the estimated attack and defense ratings and more.
summary(gm_res)

image(predict_goals(gm_res, england_2011$home[1], england_2011$visitor[1])[[1]][1:5,1:5])







km$cluster








n_features = 24
# list of parameters with initial values.
parameter_list <- list(weights_home = rep(0.1, n_features),
                       weights_away = rep(0.1, n_features),
                       bias_home=0.1,
                       bias_away=0.1,
                       rho= 0.001)
parameter_list <- list(rho= -0.3)
dc_negloglik <- function(params, goals_home, goals_visitor,
                         features, param_skeleton){
  
  # relist, to make things easier.
  plist <- relist(params, param_skeleton)
  
  # Home team expected goals
  # print(dim(as.matrix(rowSums(plist$weights_home*features))))
  lambda_home <- exp(as.matrix(rowSums(features %*% diag(scoreline_lrh$coefficients[2:25]))) + scoreline_lrh$coefficients[1])
  
  # Away team expected goals
  lambda_visitor <- exp(as.matrix(rowSums(features %*% diag(scoreline_lra$coefficients[2:25]))) + scoreline_lra$coefficients[1])
  
  # Dixon-Coles adjustment
  dc_adj <- log(tau(goals_home, goals_visitor, lambda_home, lambda_visitor, rho = plist$rho))
  hist(goals_home- train_all_targets$GH, 10)
  hist(goals_visitor- train_all_targets$GA, 10)
  hist(lambda_home- train_all_targets$predGHlr)
  hist(lambda_visitor- train_all_targets$predGAlr)
  lt = log(tau(train_all_targets$GH, train_all_targets$GA, train_all_targets$predGHlr, train_all_targets$predGAlr, rho=rho))
  hist(lt - dc_adj, 20)
  
  print(sum(is.na(dc_adj)))
  # The log-likelihood
  log_lik_home <- dpois(goals_home, lambda = lambda_home, log=TRUE)
  log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log=TRUE)
  
  log_lik <- sum((log_lik_home + log_lik_visitor + dc_adj))
  print(goals_home- train_all_targets$GH)
  
  
  print(plist$rho)
  print(mean(log(dc_adj)))
  print(head(log(dc_adj)))
  print(-1*log_lik/dim(features)[1])
  sss
  return(log_lik*-1)
  
}
optim_res <- optim(par = unlist(parameter_list), fn=dc_negloglik,
                   goals_home = train_all_targets$GH,
                   goals_visitor = train_all_targets$GA,
                   features = as.matrix(train_all_data),
                   param_skeleton=parameter_list)

# relist, and calculate the remaining parameter. 
parameter_est <- relist(optim_res$par, parameter_list)








library(tensorflow)
library(keras)
feature_size = dim(train_all_data)[2]
features <- layer_input(shape = list(feature_size))
nnmodel = keras$layers$Dense(2, activation = "exponential")
nnpreds = nnmodel(features)
nncom <- keras_model(features, list(nnpreds[,1], nnpreds[,2]))
losses = list(keras$losses$Poisson() , keras$losses$Poisson())
nncom %>% compile(optimizer='adam', loss=losses, loss_weights=list(1,1)) -> nncom
print(combined)
nncom %>% keras::fit(as.matrix(train_all_data), 
                     list(as.matrix(data.frame(train_all_targets$GH)), as.matrix(data.frame(train_all_targets$GA))),
                     # validation_data = list(as.matrix(valid_all_data),
                     # list(as.matrix(data.frame(valid_all_targets$GH)), as.matrix(data.frame(valid_all_targets$GA)))),
                     epochs = 100, 
                     batch_size = 128)
nncom %>% predict(as.matrix(train_all_data), type="response") -> tra_predsnn
train_all_targets$predGHnn = as.numeric(tra_predsnn[[1]]) 
train_all_targets$predGAnn = as.numeric(tra_predsnn[[2]])

nncom %>% predict(as.matrix(valid_all_data), type="response") -> val_predsnn
valid_all_targets$predGHnn = as.numeric(val_predsnn[[1]]) 
valid_all_targets$predGAnn = as.numeric(val_predsnn[[2]])

scoreline_nn = data.frame()

for (gghh in c(0,1,2,3,4,5,6,7,8,9)){
  for (ggaa in c(0,1,2,3,4,5,6,7,8,9)){
    this_prob = sum(dpois(gghh, valid_all_targets$predGHnn)*dpois(ggaa, valid_all_targets$predGAnn))
    scoreline_nn <- rbind(scoreline_nn, data.frame(GH = gghh, GA = ggaa, y = as.integer(this_prob)))
  }
}
scoreline_nn$yr = as.integer(scoreline_nn$y / sum(scoreline_nn$y) * 1000)/1000
ggplot(scoreline_nn, aes(GH,GA)) +
  geom_tile(aes(fill = yr), colour = "white") +
  geom_text(aes(label = yr*100), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

train_all_targets %>% mutate(LLnn = -dpois(GH, predGHnn, log=T)-dpois(GA, predGAnn, log=T)) -> train_all_targets
mean(train_all_targets$LLlr)

ggplot(data = valid_all_targets, aes(as.factor(GH), predGHnn)) + geom_violin()






custom_loss <- function(y_true, y_pred) {
  yt1 = y_true[1]
  yt2 = y_true[2]
  yp1 = y_pred[1]
  yp2 = y_pred[2]
  dct1 = tf$cast(yt1==0 & yt2==0, tf$float32)
  dct2 = tf$cast(yt1==0 & yt2==1, tf$float32)
  dct3 = tf$cast(yt1==1 & yt2==0, tf$float32)
  dct4 = tf$cast(yt1==1 & yt2==1, tf$float32)
  # rho=tf$cast(-0.1, tf$float32)
  dcadd = dct1*(1-yp1*yp2*rho)+dct2*(1+yp1*rho)+dct3*(1+yp2*rho)+dct4*(1-rho)
  p1 = (yp1 - yt1 * log(yp1))
  p2 = (yp2 - yt2 * log(yp1))
  result <- p1*p2
  return(result)
}






n_features = 12
# list of parameters with initial values.
parameter_list <- list(attack = rep(0.2, n_features),
                       defense = rep(-0.01, n_features),
                       bias_home=0.1,
                       bias_visitor = 0.1,
                       rho= 0.00)


dc_negloglik <- function(params, goals_home, goals_visitor,
                         features_home, features_visitor, param_skeleton){
  
  # relist, to make things easier.
  plist <- relist(params, param_skeleton)
  # Home team expected goals
  lambda_home <- exp(as.matrix(rowSums(plist$attack*features_home)) + as.matrix(rowSums(plist$defense*features_visitor)) + plist$bias_home)
  
  # Away team expected goals
  lambda_visitor <- exp(as.matrix(rowSums(plist$attack*features_visitor)) + as.matrix(rowSums(plist$defense*features_home)) + plist$bias_visitor)
  
  # Dixon-Coles adjustment
  dc_adj <- tau(goals_home, goals_visitor, lambda_home, lambda_visitor, rho = plist$rho)
  
  # The log-likelihood
  log_lik_home <- dpois(goals_home, lambda = lambda_home, log=TRUE)
  log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log=TRUE)
  
  log_lik <- sum((log_lik_home + log_lik_visitor + log(dc_adj)))
  
  print(plist$attack)
  print(plist$defense)
  print(-1*log_lik)
  return(log_lik*-1)
  
}

features_home = as.matrix(train_all_data)[,1:n_features]
features_visitor = as.matrix(train_all_data)[,(n_features+1):(2*n_features)]
optim_res <- optim(par = unlist(parameter_list), fn=dc_negloglik,
                   goals_home = train_all_targets$GH,
                   goals_visitor = train_all_targets$GA,
                   features_home = features_home, features_visitor = features_visitor,
                   param_skeleton=parameter_list, method = 'BFGS')

# relist, and calculate the remaining parameter. 
parameter_est <- relist(optim_res$par, parameter_list)

















library(tensorflow)
library(keras)
feature_size = dim(train_all_data)[2]
features <- layer_input(shape = list(feature_size))
dchidden = keras$layers$Dense(10, activation = "tanh")
dcmodel = keras$layers$Dense(2, activation = "exponential")
home_atts = dcmodel(features[,1:12])
away_atts = dcmodel(features[,13:24])

hi = keras$layers$Dense(1, use_bias = F, kernel_initializer = initializer_constant(1))
hg = keras$layers$Reshape(target_shape=list(as.integer(1)))(home_atts[,1]*away_atts[,2])
hg = hi(hg)[,1]
# hg = home_atts[,1]*away_atts[,2]
ag = home_atts[,2]*away_atts[,1]
combined <- keras_model(features, list(hg, ag))
losses = list(keras$losses$Poisson() , keras$losses$Poisson())
combined %>% compile(optimizer='adam', loss=losses, loss_weights=list(1,1)) -> combined
print(combined)
combined %>% keras::fit(as.matrix(train_all_data), 
                        list(as.matrix(data.frame(train_all_targets$GH)), as.matrix(data.frame(train_all_targets$GA))),
                        #validation_data = as.list(c(xval, yval)),
                        epochs = 100, 
                        batch_size = 128)

combined %>% predict(as.matrix(train_all_data), type="response") -> tra_preds
train_all_targets$predGHdc = as.numeric(tra_preds[[1]]) 
train_all_targets$predGAdc = as.numeric(tra_preds[[2]])
combined %>% predict(as.matrix(valid_all_data), type="response") -> val_preds
valid_all_targets$predGHdc = as.numeric(val_preds[[1]]) 
valid_all_targets$predGAdc = as.numeric(val_preds[[2]])

scoreline_dc = data.frame()

for (gghh in c(0,1,2,3,4,5,6,7,8,9)){
  for (ggaa in c(0,1,2,3,4,5,6,7,8,9)){
    this_prob = sum(dpois(gghh, valid_all_targets$predGHdc)*dpois(ggaa, valid_all_targets$predGAdc))
    scoreline_dc <- rbind(scoreline_dc, data.frame(GH = gghh, GA = ggaa, y = as.integer(this_prob)))
  }
}
scoreline_dc$yr = as.integer(scoreline_dc$y / sum(scoreline_dc$y) * 1000)/1000
ggplot(scoreline_dc, aes(GH,GA)) +
  geom_tile(aes(fill = yr), colour = "white") +
  geom_text(aes(label = yr*100), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

mean(dpois(valid_all_targets$GH, valid_all_targets$predGHdc))
mean(dpois(valid_all_targets$GA, valid_all_targets$predGHdc))

ggplot(data = valid_all_targets, aes(as.factor(GA), predGAdc)) + geom_violin()


s1 = 1
s2 = 1
train_all_targets %>% mutate(flag = as.numeric(GH==s1 & GA == s2)) -> validinit
validinit %>% mutate(dpro = dpois(s1, predGHbp,)*dpois(s2, predGAbp)) -> validinit

brier_dummy = verify(thresholds = seq(0,1,0.01), obs=validinit$flag, pred=validinit$dpro)
summary(brier_dummy)

reliability.plot(x=brier_dummy, xlim=c(0,0.5))





library(tensorflow)
library(keras)
feature_size = dim(train_all_data)[2]
features <- layer_input(shape = list(feature_size))
nnhidden = keras$layers$Dense(10, activation = "tanh")
nndcmodel = keras$layers$Dense(1, activation = "exponential")
nnhatts = nnhidden(features[,1:12])
nnaatts = nnhidden(features[,13:24])
nngha = keras$layers$Concatenate()(c(nnhatts[,1:5], nnaatts[,6:10]))
nngaa = keras$layers$Concatenate()(c(nnhatts[,6:10], nnaatts[,1:5]))
outgh = keras$layers$Dense(1, use_bias = FALSE)(nndcmodel(nngha))
outga = nndcmodel(nngaa)

nncom <- keras_model(features, list(outgh, outga))
losses = list(keras$losses$Poisson() , keras$losses$Poisson())
nncom %>% compile(optimizer='adam', loss=losses, loss_weights=list(1,1)) -> nncom
print(nncom)
nncom %>% keras::fit(as.matrix(train_all_data), 
                     list(as.matrix(data.frame(train_all_targets$GH)), as.matrix(data.frame(train_all_targets$GA))),
                     #validation_data = as.list(c(xval, yval)),
                     epochs = 100, 
                     batch_size = 128)


nncom %>% predict(as.matrix(train_all_data), type="response") -> tra_predsnn
train_all_targets$predGHnndc = as.numeric(tra_predsnn[[1]]) 
train_all_targets$predGAnndc = as.numeric(tra_predsnn[[2]])

nncom %>% predict(as.matrix(valid_all_data), type="response") -> val_predsnn
valid_all_targets$predGHnndc = as.numeric(val_predsnn[[1]]) 
valid_all_targets$predGAnndc = as.numeric(val_predsnn[[2]])

scoreline_nndc = data.frame()

for (gghh in c(0,1,2,3,4,5,6,7,8,9)){
  for (ggaa in c(0,1,2,3,4,5,6,7,8,9)){
    this_prob = sum(dpois(gghh, valid_all_targets$predGHnndc)*dpois(ggaa, valid_all_targets$predGAnndc))
    scoreline_nndc <- rbind(scoreline_nndc, data.frame(GH = gghh, GA = ggaa, y = as.integer(this_prob)))
  }
}
scoreline_nndc$yr = as.integer(scoreline_nndc$y / sum(scoreline_nndc$y) * 1000)/1000
ggplot(scoreline_nndc, aes(GH,GA)) +
  geom_tile(aes(fill = yr), colour = "white") +
  geom_text(aes(label = yr*100), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

mean(dpois(valid_all_targets$GH, valid_all_targets$predGHnndc))
mean(dpois(valid_all_targets$GA, valid_all_targets$predGHnndc))

ggplot(data = valid_all_targets, aes(as.factor(GH), predGHnndc)) + geom_violin()




#######
library(bivpois)
image(bivpois.table(10,10,lambda=c(4,4,0)))
data(ex4.ita91)
ex1.m8 <-lm.bp(GH~. , GA~. , l3=~. , data=cbind(train_all_data, data.frame(GH = train_all_targets$GH, GA = train_all_targets$GA)), maxit = 5)	
train_all_targets$predGHbp = ex1.m8$fitted.values$x
train_all_targets$predGAbp = ex1.m8$fitted.values$y

mean(dpois(train_all_targets$GH, train_all_targets$predGHbp))
mean(dpois(train_all_targets$GA, train_all_targets$predGAbp))
ggplot(data = train_all_targets, aes(as.factor(GH), predGHbp)) + geom_violin()



predict(ex1.m8, valid_all_data)




library(goalmodel)
library(dplyr) # Useful for data manipulation. 
library(engsoccerdata) # Some data.
library(Rcpp)

england %>% 
  filter(Season %in% c(2011),
         tier==c(1)) %>% 
  mutate(Date = as.Date(Date)) -> england_2011

# Fit the default model, with the home team as team1.

gm_res <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                    team1 = england_2011$home, team2=england_2011$visitor)

# Show the estimated attack and defense ratings and more.
summary(gm_res)

image(predict_goals(gm_res, england_2011$home[1], england_2011$visitor[1])[[1]][1:5,1:5])







library(bivpois)
data(ex4.ita91)
form1 <- ~c(team1,team2)+c(team2,team1)
ex4.m1<-lm.dibp( GH~., GA~., data=cbind(train_all_data, data.frame(GH = train_all_targets$GH, GA = train_all_targets$GA)), maxit=20, distribution="geometric")
ex4.m1$theta
ex4.m1$coef     # all parameters
ex4.m1$beta1    # model parameters for lambda1
ex4.m1$beta2    # model parameters for lambda2. 
# All are the same as in beta1 except the intercept
ex4.m1$beta2[1] # Intercpept for lambda2. 
ex4.m1$beta2[1]-ex4.m1$beta2[2]	# estimated home effect

# estimating the effect for 18th level of attack (team1..team2) [Verona]
-sum(ex4.m1$coef[ 2:18]) 
# estimating the effect for 18th level of defence(team2..team1) [Verona]
-sum(ex4.m1$coef[19:35]) 
#







require(engsoccerdata)

england %>% 
  filter(Season == 2011,
         tier==1) %>% 
  mutate(home = as.character(home),
         visitor = as.character(visitor))-> england_2011

all_teams <- sort(unique(c(england_2011$home, england_2011$visitor)), decreasing = FALSE)
n_teams <- length(all_teams)







library(tensorflow)
library(keras)
feature_size = dim(train_all_data)[2]
features <- layer_input(shape = list(feature_size))
dchidden = keras$layers$Dense(10, activation = "tanh")
dcmodel = keras$layers$Dense(2, activation = "exponential", kernel_initializer = initializer_constant(0.1))
home_atts = dcmodel(features[,1:12])
away_atts = dcmodel(features[,13:24])

hi = keras$layers$Dense(1, use_bias = F, kernel_initializer = initializer_constant(1))
hg = keras$layers$Reshape(target_shape=list(as.integer(1)))(home_atts[,1]*away_atts[,2])
hg = hi(hg)[,1]
ag = home_atts[,2]*away_atts[,1]
combined <- keras_model(features, list(hg, ag))
# rho=-0.032
losses = list(keras$losses$Poisson() , keras$losses$Poisson())
combined %>% compile(optimizer='adam', loss=losses) -> combined
print(combined)

XT = as.matrix(train_all_data)
YT = list(as.matrix(data.frame(train_all_targets$GH)), as.matrix(data.frame(train_all_targets$GA)))
XV = as.matrix(valid_all_data)
YV = list(as.matrix(data.frame(valid_all_targets$GH)), as.matrix(data.frame(valid_all_targets$GA)))
es = EarlyStopping = keras$callbacks$EarlyStopping(
  monitor="val_loss",
  min_delta=0,
  patience=100,
  verbose=0,
  mode="auto",
  restore_best_weights=TRUE)
combined %>% keras::fit(XT, 
                        YT,
                        validation_data = list(XV, YV),
                        callbacks=list(es),
                        epochs = 500, 
                        batch_size = 128)



combined %>% predict(as.matrix(train_all_data), type="response") -> tra_preds
train_all_targets$predGHdc = as.numeric(tra_preds[[1]]) 
train_all_targets$predGAdc = as.numeric(tra_preds[[2]])
combined %>% predict(as.matrix(valid_all_data), type="response") -> val_preds
valid_all_targets$predGHdc = as.numeric(val_preds[[1]]) 
valid_all_targets$predGAdc = as.numeric(val_preds[[2]])

scoreline_dc = data.frame()
rho = 0
for (gghh in c(0,1,2,3,4,5,6,7,8,9)){
  for (ggaa in c(0,1,2,3,4,5,6,7,8,9)){
    if (gghh == 0 & ggaa == 0){
      dct = (1-valid_all_targets$predGHdc*valid_all_targets$predGAdc*rho)
    }
    if (gghh == 0 & ggaa == 1){
      dct = (1+valid_all_targets$predGHdc*rho)
    }
    if (gghh == 1 & ggaa == 0){
      dct = (1-valid_all_targets$predGAdc*rho)
    }
    if (gghh == 1 & ggaa == 1){
      dct = (1-rho)
    }
    this_prob = sum(dpois(gghh, valid_all_targets$predGHdc)*dpois(ggaa, valid_all_targets$predGAdc)*dct)
    scoreline_dc <- rbind(scoreline_dc, data.frame(GH = gghh, GA = ggaa, y = as.integer(this_prob)))
  }
}
scoreline_dc$yr = as.integer(scoreline_dc$y / sum(scoreline_dc$y) * 1000)/1000
ggplot(scoreline_dc, aes(GH,GA)) +
  geom_tile(aes(fill = yr), colour = "white") +
  geom_text(aes(label = yr*100), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")


for (i in 1:dim(train_all_targets)[1]){
  tgh = train_all_targets$GH[i]
  tga = train_all_targets$GA[i]
  pgh = train_all_targets$predGHdc[i]
  pga = train_all_targets$predGAdc[i]
  ploss = dpois(tgh, pgh)*dpois(tga, pga)
  train_all_targets$LLdc[i] = ploss*tau(tgh, tga, pgh, pga, 0.003)
}
mean(train_all_targets$LLdc)
mean(dpois(valid_all_targets$GH, valid_all_targets$predGHdc))
mean(dpois(valid_all_targets$GA, valid_all_targets$predGHdc))

ggplot(data = valid_all_targets, aes(as.factor(GA), predGAdc)) + geom_violin()










library(tensorflow)
library(keras)
feature_size = dim(train_all_data)[2]
features <- layer_input(shape = list(feature_size))
dchidden = keras$layers$Dense(10, activation = "tanh")
dcmodel = keras$layers$Dense(3, activation = "exponential", kernel_initializer = initializer_constant(0.1))
home_atts = dcmodel(features[,1:12])
away_atts = dcmodel(features[,13:24])
rhoo = home_atts[,3]*away_atts[,3]
hi = keras$layers$Dense(1, use_bias = F, kernel_initializer = initializer_constant(1))
hg = keras$layers$Reshape(target_shape=list(as.integer(1)))(home_atts[,1]*away_atts[,2])
hg = hi(hg)[,1]
ag = home_atts[,2]*away_atts[,1]
combined <- keras_model(features, list(hg+rhoo, ag+rhoo))
losses = list(keras$losses$Poisson() , keras$losses$Poisson())
combined %>% compile(optimizer='adam', loss=losses) -> combined
print(combined)

XT = as.matrix(train_all_data)
YT = list(as.matrix(data.frame(train_all_targets$GH)), as.matrix(data.frame(train_all_targets$GA)))
XV = as.matrix(valid_all_data)
YV = list(as.matrix(data.frame(valid_all_targets$GH)), as.matrix(data.frame(valid_all_targets$GA)))
es = EarlyStopping = keras$callbacks$EarlyStopping(
  monitor="val_loss",
  min_delta=0,
  patience=100,
  verbose=0,
  mode="auto",
  restore_best_weights=TRUE)
combined %>% keras::fit(XT, 
                        YT,
                        validation_data = list(XV, YV),
                        callbacks=list(es),
                        epochs = 500, 
                        batch_size = 128)




























M25lr_train = train_all_targets$pred25lr

brier_lr25pred = verify(thresholds = seq(0,1,0.04), obs=M25target_train, pred=M25lr_train)
summary(brier_lr25pred)

reliability.plot(x=brier_lr25pred$y.i, obar.i = as.matrix(data.frame(brier_Mx25pstar$obar.i, brier_lr25pred$obar.i)), prob.y = as.matrix(data.frame(brier_Mx25pstar$prob.y, brier_lr25pred$prob.y)))



















sag = cbind(train_all_data, train_all_targets$GT)
spec <- feature_spec(sag, HFG ~ . )
input = layer_input(batch_shape=c(24))
output = input %>% layer_dens











# pm = glm(`train_targets$GT` ~ ., family="poisson", data = cbind(train_normalized, train_targets$GT))
# pm = lm(1/`train_targets$Bb25` ~ ., data = cbind(train_normalized, train_targets$Bb25))
pm = glm(as.factor(`train_targets$GT` > 2.5) ~ ., data = cbind(train_normalized, train_targets$GT), family = "binomial")
summary(pm)

# VGT = predict(pm, valid_normalized, type="response")
VBb25 = predict(pm, valid_normalized, type="response")

hist(pm$fitted.values)
hist(valid_targets$GT - VGT)
plot(valid_targets$GT, VGT)


# ggplot(cbind(valid_targets, as.data.frame(VGT)), aes(x=GT, y=VGT)) + geom_point()
ggplot(cbind(valid_targets, as.data.frame(VBb25)), aes(x=1/Bb25, y=VBb25, color = sign(GT>2.5))) + geom_point()
ggplot(cbind(valid_targets, as.data.frame(VBb25)), aes(x=as.factor(GT>2.5), y=1/Bb25, color = sign(GT>2.5))) + geom_boxplot(notch=FALSE)

ggplot(cbind(valid_targets, as.data.frame(VBb25)), aes(x=as.factor(GT>2.5), y=VBb25, color = sign(GT>2.5))) + geom_boxplot(notch=FALSE)
ggplot(cbind(valid_targets, as.data.frame(VBb25)), aes(x=as.factor(GT>2.5), y=VBb25, color = sign(GT>2.5))) + geom_boxplot(notch=FALSE)

library(rel)



















## LINEAR REGRESSION
n=nrow(train)
K=100
BETA=NULL
rmse=NULL
for(i in c(1:K))
{
  sel=sort(sample(c(1:n),n-10,replace=FALSE))
  data0_bis=train[sel,]
  Y0=data0_bis$GT
  X0=as.matrix(data0_bis[,-ncol(train)])
  beta=solve(crossprod(X0))%*%t(X0)%*%Y0
  BETA=cbind(BETA,beta) 
  
  Y1=valid$GT
  X1=as.matrix(valid[,-ncol(valid)])
  reg.forecast=X1%*%beta
  rmse=c(rmse,sqrt(mean((Y1-reg.forecast)^2))) 
}

matplot(BETA,type='l')
plot(BETA[1,],type='l')
plot(BETA[2,],type='l')
boxplot(BETA[1,])
boxplot(as.data.frame(t(BETA)))
hist(BETA[1,],breaks=10)


plot(sort(rmse/sd(Y1)),type='l')
# norm=colSums(BETA^2)
# plot(norm,type='l')


### RIDGE REGRESSION
p=ncol(X0)
# p=5
Y0=train$GT
X0=as.matrix(train[,-ncol(train)])
# X0=as.matrix(train[,1:5])

lambda=50
betaRidge=solve(crossprod(X0)+lambda*diag(p))%*%t(X0)%*%Y0
HRidge=crossprod(t(X0),solve(crossprod(X0)+lambda*diag(p)))%*%t(X0)
ychap_ridge=X0%*%betaRidge
sum(diag(HRidge))


n=nrow(train)
K=100
BETAridge=NULL
rmseridge=NULL
for(i in c(1:K))
{
  sel=sort(sample(c(1:n),n-10,replace=FALSE))
  data0_bis=train[sel,]
  Y0=data0_bis$GT
  X0=as.matrix(data0_bis[,-ncol(train)])
  # X0=as.matrix(data0_bis[,1:5])
  beta=solve(crossprod(X0)+lambda*diag(p))%*%t(X0)%*%Y0
  BETAridge=cbind(BETAridge,beta) 
  
  Y1=valid$GT
  X1=as.matrix(valid[,-ncol(valid)])
  # X1=as.matrix(valid[,1:5])
  reg.forecast=X1%*%beta
  rmseridge=c(rmseridge,sqrt(mean((Y1-reg.forecast)^2))) 
}

par(mfrow=c(2,1))
matplot(BETA,type='l',ylim=range(BETA,BETAridge))
matplot(BETAridge,type='l',ylim=range(BETA,BETAridge))

par(mfrow=c(1,1))
plot(rmse/sd(Y0),type='l',ylim=range(rmse/sd(Y0),rmseridge/sd(Y0)))
lines(rmseridge/sd(Y0),col='red')
legend('top',col=c("black","red"),c("Reg. lineaire","ridge"),lty=1,bty='n',lwd=2)


plot(reg.forecast, valid$GT)

high_pred = valid$GT[reg.forecast>2.5]
score = sum(high_pred>2.5)/length(high_pred)
score

hist(valid$GT)
hist(reg.forecast)

pm = glm(GT ~ ., family="poisson", data=train)
pp = predict(pm, valid, type="response")
hist(pp)
plot(reg.forecast, valid$GT)
high_pred = valid$GT[pp>2.5]
score = sum(high_pred>2.5)/length(high_pred)
score





















### GAM
library(mgcv)
library(mgcViz)
library(gridExtra)
library(yarrr)
rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
}
attach(train)
plot(GT,type='l')
plot(HFG_mean, GT,pch=16,cex=0.5)
plot(HS_mean, GT,pch=16,cex=0.5)
acf(GT, lag.max=20)

Nblock<-10
borne_block<-seq(1, nrow(train), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))


gam1<-gam(GT~s(HFG_mean,k=10)+s(HS_mean,k=10)+s(AFG_mean,k=10)+s(AS_mean,k=10), data=train)
summary(gam1)  

blockRMSE<-function(equation, block)
{
  g<- gam(as.formula(equation), data=train[-block,])
  forecast<-predict(g, newdata=train[block,])
  return(train[block,]$Load-forecast)
} 

equation <- GT~s(HFG_mean,k=10)+s(HS_mean,k=10)+s(AFG_mean,k=10)+s(AS_mean,k=10)
Block_residuals<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc1<-rmse(Block_residuals)
boxplot(Block_residuals)
plot(Block_residuals, type='l')

plot(data0$Temp, Block_residuals, pch=16)
plot(data0$NumWeek, Block_residuals, pch=16)
plot(data0$Load1, Block_residuals, pch=16)

gam_prov <- gam(Block_residuals~s(Load1), data=data0)  
summary(gam_prov)
fit <- getViz(gam_prov, nsim = 50)
plot(sm(fit, 1), n = 400) + l_points() + l_fitLine() + l_ciLine()


equation <- Load~s(Time,k=3)+s(NumWeek,k=30)+s(Temp,k=5)+s(Load1, k=10)
gam2<-gam(equation, data=data0)
summary(gam2)
Block_residuals2<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc2<-rmse(Block_residuals2)
rmseBloc2

plot(data0$IPI, Block_residuals2, pch=16)
gam_prov <- gam(Block_residuals2~s(data0$IPI))   
summary(gam_prov)

plot(data0$IPI_CVS, Block_residuals2, pch=16)
gam_prov <- gam(Block_residuals2~s(data0$IPI_CVS))   
summary(gam_prov)
plot(gam_prov)

equation <- Load~s(Time,k=3)+s(NumWeek,k=30)+s(Temp,k=5)+s(Load1, k=10)+s(IPI_CVS)
gam3<-gam(equation, data=data0)
summary(gam3)
Block_residuals3<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc3<-rmse(Block_residuals3)
rmseBloc3

#####change the IPI_CVS in linear effect
equation <- Load~s(Time,k=3)+s(NumWeek,k=30)+s(Temp,k=5)+s(Load1, k=10)+IPI_CVS
gam4<-gam(equation, data=data0)
summary(gam4)
Block_residuals4<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc4<-rmse(Block_residuals4)
rmseBloc4


plot(data0$Temp1, Block_residuals4, pch=16)
gam_prov <- gam(Block_residuals4~s(data0$Temp1))   
summary(gam_prov)
plot(gam_prov)

equation <- Load~s(Time,k=3)+s(NumWeek,k=30)+s(Temp,k=5)+s(Load1, k=10)+IPI_CVS+s(Temp1)
gam5<-gam(equation, data=data0)
summary(gam5)
Block_residuals5<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc5<-rmse(Block_residuals5)
rmseBloc5

plot(gam5$residuals, type='l')

noel = which(abs(data0$Day - 24) <= 3 & data0$Month == 12)
consoNoel = vector("numeric", length(data0$Time))
consoNoel[noel] = 1
data0 <- data.frame(data0, consoNoel)

plot(data0$Time, gam5$residuals, type='l')
select<-which(data0$consoNoel==1)
points(data0$Time[select], gam5$residuals[select], col='red', pch=20)

equation <- Load~s(Time,k=3)+s(NumWeek,k=30, bs='cc')+s(Temp,k=5)+s(Load1, k=10)+IPI_CVS+s(Temp1)
+consoNoel
gam6<-gam(equation, data=data0)
summary(gam6)
Block_residuals6<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc6<-rmse(Block_residuals6)
rmseBloc6

plot(data0$Time, gam6$residuals, type='l')
select<-which(data0$consoNoel==1)
points(data0$Time[select], gam6$residuals[select], col='red', pch=20)

data0[which(abs(gam6$residuals)>3000), 1:3]

noel = which(abs(data1$Day - 24) <= 3 & data1$Month == 12)
consoNoel = vector("numeric", length(data1$Time))
consoNoel[noel] = 1
data1 <- data.frame(data1, consoNoel)


ychap6 <- predict(gam6, newdata=data1)
rmse(data1$Load-ychap)

equation <- Load~s(Time,k=3)+s(NumWeek,k=30, bs='cc')+te(Time, Temp, k=c(3, 5))+s(Load1, k=10)+IPI_CVS+s(Temp1)

gam7<-gam(equation, data=data0)
summary(gam7)
ychap <- predict(gam7, newdata=data1)
rmse(data1$Load-ychap)

par(mfrow=c(1,1))
plot(data1$Load, type='l')
lines(ychap, col='red')
lines(ychap6, col='blue')






# head(sort(all_data$Date, decreasing = TRUE))
# plot(FTHG ~ Date, all_data, xaxt = "n", type = "l")

colnames(all_data)
#GOALS
par(mfrow=c(1,3))
hist(all_data$FTHG)
hist(as.numeric(all_data$FTR))
hist(all_data$FTHG-as.numeric(all_data$FTR))
#CORNERS
par(mfrow=c(1,3))
hist(all_data$HC, breaks = 20)
hist(all_data$AC, breaks = 20)
hist(all_data$HC - all_data$AC, breaks = 20)
#FOULS
par(mfrow=c(1,3))
hist(all_data$HF, breaks = 20)
hist(all_data$AF, breaks = 20)
hist(all_data$HF - all_data$AF, breaks = 20)





# paste("Ali", "Baba", sep = "")
# matches <- read.csv(file = 'E0.csv')
# matches2 <- filter(matches$FTR, as.numeric(matches$B365H) < 3)
# res <- matches$FTR[as.numeric(matches$B365H) < 3 & as.numeric(matches$B365H) > 2.5 ]
# sum(res == "H" )/ length(res)
# length(res)
# 
# library(httr)
# 
# url <- "https://api-football-v1.p.rapidapi.com/v3/timezone"
# 
# response <- VERB("GET", url, add_headers(x_rapidapi_host = 'api-football-v1.p.rapidapi.com', x_rapidapi_key = 'f33ef3a56bmsh8afb532b2af65e3p156ca6jsn02d53038715e'), content_type("application/octet-stream"))
#                                          
# content(response, "text")