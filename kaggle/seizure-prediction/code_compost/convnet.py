from keras.optimizers import Adam, SGD
from keras.models import Model
from keras.layers.advanced_activations import ELU, LeakyReLU
from keras.layers import Input, merge, Convolution1D, MaxPooling1D
from keras.layers import UpSampling1D, Dense, Flatten
from keras.layers.normalization import BatchNormalization
from keras.layers.core import Activation
from keras.layers.core import Dropout
from keras.regularizers import WeightRegularizer

x1, x2, x3, x4 = load("data/splits.npz")

#TODO: investigate pooling layers and residual layers
T = X.shape[1]

inputs = Input(shape = (T, 16), name = 'eeg_input')

l2_regularizer = WeightRegularizer(l2 = .17)
# 4 channels of output, window size 5
down_conv1 = Convolution1D(4, 8, init = 'he_normal',
                           bias = True,
                           border_mode = 'same',
                           #W_regularizer = l2_regularizer
                           )(inputs)
down_conv1 = MaxPooling1D(2, 2)(down_conv1)

down_conv1 = BatchNormalization()(down_conv1)
down_conv1 = Activation('sigmoid')(down_conv1)
down_conv1 = Dropout(.1)(down_conv1)

down_conv2 = Convolution1D(2, 16, init = 'he_normal',
                           bias = True,
                           border_mode = 'same',
                           #W_regularizer = l2_regularizer
                           )(down_conv1)
down_conv2 = BatchNormalization()(down_conv2)
down_conv2 = Activation('sigmoid')(down_conv2)
down_conv2 = Dropout(.2)(down_conv2)

down_conv3 = Convolution1D(1, 32, init = 'he_normal',
                           bias = True,
                           border_mode = 'same',
                           #W_regularizer = l2_regularizer
                           )(down_conv2)

down_conv3 = BatchNormalization()(down_conv3)
down_conv3 = Activation('sigmoid')(down_conv3)
down_conv3 = Dropout(.3)(down_conv3)

#z = theano.tensor.sum(inputs, axis = 1)

pre = Flatten()(down_conv3)

pre = Flatten()(down_conv3)

print(pre._keras_shape)
l2_regularizer2 = WeightRegularizer(l2 = .001)
output_layer = Dense(1, bias = True, init = 'he_normal')(pre)

output_layer = BatchNormalization()(output_layer)
output_layer = Activation('sigmoid')(output_layer)

model = Model(input = inputs, output = output_layer)

sgd_optimizer = SGD(lr = 0.001, momentum = .9, decay = .0005)
model.compile(optimizer = sgd_optimizer,
              loss = 'binary_crossentropy')

hist = model.fit(X_train, y_train,
                 validation_data = (X_test, y_test),
                 batch_size = 60, nb_epoch = 30,
                 verbose = 1, shuffle = True)

model.predict(X_test)
test_predictions = model.predict(X_test)
test_predictions[1:20]
roc_auc_score(y_test, test_predictions)

# Manual model exploration

weights = model.get_weights()
from theano import function
f1 = function([model.layers[0].input], model.layers[1].output)
out1 = f1(X_test)

f2 = function([model.layers[2].input], model.layers[5].output)
out2 = f2(f1(X_test))

# All the way through
f5 = function([model.layers[0].input], model.layers[5].output)
out5 = f5(X_test)

# Manual exploration
weights = model.get_weights()
from theano import function
f1 = function([model.layers[0].input], model.layers[1].output)
out1 = f1(X_test)

f2 = function([model.layers[2].input], model.layers[5].output)
out2 = f2(f1(X_test))

# All the way through
f5 = function([model.layers[0].input], model.layers[5].output)
out5 = f5(X_test)


