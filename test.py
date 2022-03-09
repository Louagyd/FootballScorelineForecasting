import pyreadr as pr
import numpy as np

all_data = pr.read_r("datalast/all_data.Rda")
all_data = np.asarray(all_data["all_data"])
all_targets = pr.read_r("datalast/all_targets.Rda")
all_targets = np.asarray(all_targets["all_targets"])

num_data = len(all_data)
num_train = int(num_data*0.7)
num_valid = int(num_data*0.15)
num_test = num_data-num_train-num_valid

v = np.random.permutation(len(all_data))
train = all_data[v[:num_train]]
valid = all_data[v[num_train:(num_train+num_valid)]]
test = all_data[v[(num_train+num_valid):]]
train_t = all_targets[v[:num_train]]
valid_t = all_targets[v[num_train:(num_train+num_valid)]]
test_t = all_targets[v[(num_train+num_valid):]]

print(len(train), len(valid), len(test))
print(len(train_t), len(valid_t), len(test_t))
print(train_t.shape)

import models
from sklearn import linear_model
clf_H = linear_model.PoissonRegressor()
clf_H.fit(train, train_t[:,0])
pred_H = clf_H.predict(test)
clf_A = linear_model.PoissonRegressor()
clf_A.fit(train, train_t[:,1])
pred_A = clf_A.predict(test)

loss = models.evaluate(test_t, [pred_H, pred_A])
print("Independent Poisson Regression Loss: ", loss)

S1 = models.ScorelinePredictor(train, train_t, valid, valid_t, test, test_t, type="bivariate", verbose=0)
S2 = models.ScorelinePredictor(train, train_t, valid, valid_t, test, test_t, type="generalrating", verbose=0)
S3 = models.ScorelinePredictor(train, train_t, valid, valid_t, test, test_t, type="attackdefense", verbose=0)
S4 = models.ScorelinePredictor(train, train_t, valid, valid_t, test, test_t, type="aaa", verbose=0)
S5 = models.ScorelinePredictor(train, train_t, valid, valid_t, test, test_t, type="bbb", verbose=0)
S6 = models.ScorelinePredictor(train, train_t, valid, valid_t, test, test_t, type="ccc", verbose=0)


#
# import tensorflow as tf
#
# feature_size = 24
#
# features = tf.keras.layers.Input(shape=[feature_size])
# features_home = features[:,:11]
# features_away = features[:,12:23]
# losses = [tf.keras.losses.Poisson(name = "LossHome"), tf.keras.losses.Poisson(name = "LossAway")]
#
#
