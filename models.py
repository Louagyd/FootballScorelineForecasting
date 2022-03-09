import tensorflow as tf
from keras.utils.layer_utils import count_params

import numpy as np
from scipy.stats import poisson


def evaluate(test_targets, test_preds):
    res_H, res_A = test_preds
    p1 = np.mean(np.log(poisson.pmf(test_targets[:, 0], res_H)))
    p2 = np.mean(np.log(poisson.pmf(test_targets[:, 1], res_A)))
    return -p1 - p2

class ScorelinePredictor():
    def __init__(self, train_data, train_targets,
                 validation_data = None, validation_targets = None,
                 test_data = None, test_targets = None, type = "bivariate", verbose=1):
        self.feature_size = train_data.shape[-1]
        self.features = tf.keras.layers.Input(shape=[self.feature_size], name="features")
        self.features_home = self.features[:, :int(self.feature_size/2)]
        self.features_away = self.features[:, int(self.feature_size/2):]
        self.losses = [tf.keras.losses.Poisson(name="LossHome"), tf.keras.losses.Poisson(name="LossAway")]
        self.regularizer = tf.keras.regularizers.L2(0.0005)
        self.earlystopping = tf.keras.callbacks.EarlyStopping(
            monitor="val_loss",
            patience=100,
            restore_best_weights=1)
        self.tensorboard = tf.keras.callbacks.TensorBoard(
            log_dir="logs",
            write_graph=True)
        print("training model ", type)
        if type == "bivariate":
            self.model = self.bivariate_poisson()
        if type == "attackdefense":
            self.model = self.general_rating(2)
        if type == "generalrating":
            self.model = self.general_rating(1)
        if type == "aaa":
            self.model = self.general_rating(3)

        if type == "bbb":
            self.model = self.general_rating(4)
        if type == "ccc":
            self.model = self.general_rating(5)


        self.fit(train_data, train_targets, validation_data, validation_targets, verbose=verbose)
        res = self.test_loss(test_data, test_targets)
        print("test loss", res)


    def bivariate_poisson(self):
        nnhidden = tf.keras.layers.Dense(10, activation="tanh",
                                         kernel_regularizer=self.regularizer,
                                         bias_regularizer=self.regularizer)
        nngoals = tf.keras.layers.Dense(3, activation="exponential",
                                        kernel_regularizer=self.regularizer,
                                        bias_regularizer=self.regularizer)
        nnpreds = nngoals(self.features)
        nnbp = tf.keras.Model(self.features, [nnpreds[:,0]+nnpreds[:,2], nnpreds[:,1]+nnpreds[:,2]])
        return nnbp

    def bivariate_poisson2(self):
        nnhidden = tf.keras.layers.Dense(4, activation="tanh",
                                         kernel_regularizer=self.regularizer,
                                         bias_regularizer=self.regularizer)
        nnhidden2 = tf.keras.layers.Dense(4, activation="relu",
                                          kernel_regularizer=self.regularizer,
                                          bias_regularizer=self.regularizer)
        # nndcmodel = keras.layers.Dense(1, activation = "exponential")
        nnhatts = nnhidden(self.features_home)
        nnaatts = nnhidden(self.features_away)
        nngha = tf.keras.layers.Concatenate()([nnhatts, nnaatts])
        outgh = tf.keras.layers.Dense(3, activation="exponential",
                                      kernel_regularizer=self.regularizer,
                                      bias_regularizer=self.regularizer)(nngha)
        nncom = tf.keras.Model(self.features, [outgh[:, 0] + outgh[:,2], outgh[:, 1]+outgh[:,2]])
        return nncom

    def general_rating(self, nhidden = 10):
        nnhidden = tf.keras.layers.Dense(nhidden, activation="tanh",
                                         kernel_regularizer=self.regularizer,
                                         bias_regularizer=self.regularizer)
        nnhatts = nnhidden(self.features_home)
        nnaatts = nnhidden(self.features_away)
        nngha = tf.keras.layers.Concatenate()([nnhatts, nnaatts])
        outgh = tf.keras.layers.Dense(2, activation="exponential",
                                         kernel_regularizer=self.regularizer,
                                         bias_regularizer=self.regularizer)(nngha)

        nncom = tf.keras.Model(self.features, [outgh[:, 0], outgh[:, 1]])
        return nncom

    def attack_defense(self):
        nnhidden = tf.keras.layers.Dense(4, activation="tanh",
                                         kernel_regularizer=self.regularizer,
                                         bias_regularizer=self.regularizer)
        nnhidden2 = tf.keras.layers.Dense(4, activation="relu",
                                         kernel_regularizer=self.regularizer,
                                         bias_regularizer=self.regularizer)
        # nndcmodel = keras.layers.Dense(1, activation = "exponential")
        nnhatts = nnhidden(self.features_home)
        nnaatts = nnhidden(self.features_away)
        nngha = tf.keras.layers.Concatenate()([nnhatts[:, :2], nnaatts[:, 2:]])
        nngaa = tf.keras.layers.Concatenate()([nnaatts[:, :2], nnhatts[:, 2:]])
        outgh = tf.keras.layers.Dense(1, activation="exponential",
                                         kernel_regularizer=self.regularizer,
                                         bias_regularizer=self.regularizer)(nngha)
        outga = tf.keras.layers.Dense(1, activation="exponential",
                                         kernel_regularizer=self.regularizer,
                                         bias_regularizer=self.regularizer)(nngaa)

        nncom = tf.keras.Model(self.features, [outgh[:,0], outga[:,0]])
        return nncom


    def fit(self, train_data, train_targets, validation_data, validation_targets, verbose=1):
        self.model.compile(optimizer='adam', loss=self.losses, loss_weights=[1, 1])
        trainable_count = count_params(self.model.trainable_weights)
        print("num parameters: ", trainable_count)
        history = self.model.fit(train_data, [train_targets[:, 0], train_targets[:, 1]],
                                 validation_data=[validation_data,
                                                  [validation_targets[:, 0], validation_targets[:, 1]]],
                                 epochs=500,
                                 callbacks=[self.earlystopping, self.tensorboard],
                                 batch_size=128,
                                 verbose=verbose)
        return history

    def test_loss(self, test_data, test_targets):
        res_H, res_A = self.model.predict(test_data)
        loss = evaluate(test_targets, [res_H, res_A])
        return loss