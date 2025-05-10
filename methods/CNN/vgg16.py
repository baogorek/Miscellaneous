# https://youtu.be/vgdFovAZUzM
# AND: https://github.com/bnsreenu/python_for_microscopists/blob/master/159b_VGG16_imagenet_weights_RF_for_semantic.py
# plus modifications by bogorek (me) to get this to work in March 2023
# Data found here: https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqbm94LUYwMVNFaFVLU1Q5UklMaFFtTDNUUDA3UXxBQ3Jtc0tsblRwLVloX0gwbU1iblF2bjNENkNraTVCc2J4OFZWbHB6Z2RYYWlkMWNEVG5lWm1pWlRBdmxNT3NNT0szbmxqd3EyZE93YlB1a1QwWWMtQktVX1VFRjJqQzA3YUFnRS1MOHcta3A1SFhvdWJSYng5MA&q=https%3A%2F%2Fdrive.google.com%2Ffile%2Fd%2F1HWtBaSa-LTyAMgf2uaz1T9o1sTWDBajU%2Fview%3Fusp%3Dsharing&v=vgdFovAZUzM

"""
@author: Sreenivas Bhattiprolu
Annotate images at www.apeer.com to create labels. 
Code last tested on: 
    Tensorflow: 2.2.0
    Keras: 2.3.1
    Python: 3.7
"""

import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
import glob
import cv2
import pickle

from PIL import Image
from keras.models import Sequential, Model
from keras.layers import Conv2D
import os
from keras.applications.vgg16 import VGG16

train_dir = "/devl/data/sandstone_data_for_ML/separate_labels_for_each_class/"

print(os.listdir(train_dir))

#Resizing images is optional, CNNs are ok with large images
SIZE_X = 1024 #Resize images (height  = X, width = Y)
SIZE_Y = 996

#Capture training image info as a list
train_images = []
tif_path = os.path.join(train_dir, "9_train_images_sandstone.tif")
ret, images = cv2.imreadmulti(tif_path, [], cv2.IMREAD_COLOR)
for i in range(len(images)):
    img = images[i]
    img = cv2.resize(img, (SIZE_Y, SIZE_X))
    img = cv2.cvtColor(img, cv2.COLOR_RGB2BGR)
    train_images.append(img)
#train_labels.append(label)
#Convert list to array for machine learning processing        
train_images = np.array(train_images)

#Capture mask/label info as a list
train_masks = [] 
tif_path = os.path.join(train_dir, "9_train_images_sandstone_class4.ome.tiff")
ret, images = cv2.imreadmulti(tif_path, [], cv2.IMREAD_GRAYSCALE)
for i in range(len(images)):
    img = images[i]
    img = cv2.resize(img, (SIZE_Y, SIZE_X))
    train_masks.append(img)
#Convert list to array for machine learning processing          
train_masks = np.array(train_masks)

# look at the last mask
cv2.imshow('Image', img)

#Use customary x_train and y_train variables
X_train = train_images
y_train = train_masks
y_train = np.expand_dims(y_train, axis=3) #May not be necessary.. leftover from previous code 


#Load VGG16 model wothout classifier/fully connected layers
#Load imagenet weights that we are going to use as feature generators
VGG_model = VGG16(weights='imagenet', include_top=False, input_shape=(SIZE_X, SIZE_Y, 3))

#Make loaded layers as non-trainable. This is important as we want to work with pre-trained weights
for layer in VGG_model.layers:
	layer.trainable = False
    
VGG_model.summary()  #Trainable parameters will be 0

#After the first 2 convolutional layers the image dimension changes. 
#So for easy comparison to Y (labels) let us only take first 2 conv layers
#and create a new model to extract features
#New model with only first 2 conv layers
new_model = Model(inputs=VGG_model.input, outputs=VGG_model.get_layer('block1_conv2').output)
new_model.summary()

#Now, let us apply feature extractor to our training data
features = new_model.predict(X_train)

#Plot features to view them
square = 8
ix=1
for _ in range(square):
    for _ in range(square):
        ax = plt.subplot(square, square, ix)
        ax.set_xticks([])
        ax.set_yticks([])
        plt.imshow(features[0,:,:,ix-1], cmap='gray')
        ix +=1
plt.show()

#Reassign 'features' as X to make it easy to follow
X=features
X = X.reshape(-1, X.shape[3])  #Make it compatible for Random Forest and match Y labels

#Reshape Y to match X
Y = y_train.reshape(-1)

#Combine X and Y into a dataframe to make it easy to drop all rows with Y values 0
#In our labels Y values 0 = unlabeled pixels. 
dataset = pd.DataFrame(X)
dataset['Label'] = Y
print(dataset['Label'].unique())
print(dataset['Label'].value_counts())

##If we do not want to include pixels with value 0 
##e.g. Sometimes unlabeled pixels may be given a value 0.
dataset = dataset[dataset['Label'] != 0]

#Redefine X and Y for Random Forest
X_for_RF = dataset.drop(labels = ['Label'], axis=1)
Y_for_RF = dataset['Label']

#RANDOM FOREST
from sklearn.ensemble import RandomForestClassifier
model = RandomForestClassifier(n_estimators = 50, random_state = 42)

# Train the model on training data
model.fit(X_for_RF, Y_for_RF) 

#Save model for future use
filename = 'RF_model.sav'
pickle.dump(model, open(filename, 'wb'))

#Load model.... 
loaded_model = pickle.load(open(filename, 'rb'))

#Test on a different image
#READ EXTERNAL IMAGE...
test_img = cv2.imread('images/test_images/Sandstone_Versa0360.tif', cv2.IMREAD_COLOR)       
test_img = cv2.resize(test_img, (SIZE_Y, SIZE_X))
test_img = cv2.cvtColor(test_img, cv2.COLOR_RGB2BGR)
test_img = np.expand_dims(test_img, axis=0)

#predict_image = np.expand_dims(X_train[8,:,:,:], axis=0)
X_test_feature = new_model.predict(test_img)
X_test_feature = X_test_feature.reshape(-1, X_test_feature.shape[3])

prediction = loaded_model.predict(X_test_feature)

#View and Save segmented image
prediction_image = prediction.reshape(mask.shape)
plt.imshow(prediction_image, cmap='gray')
plt.imsave('images/test_images/360_segmented.jpg', prediction_image, cmap='gray')
