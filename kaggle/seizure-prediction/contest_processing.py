import os
#os.chdir("C:/devl/kaggle/seizure-prediction")

import numpy as np
import pandas as pd
import ntpath

import matplotlib
import matplotlib.pyplot as plt

from collections import Counter
import itertools

from sklearn.cross_validation import train_test_split
from matplotlib.backends.backend_pdf import PdfPages
from util import *

train_dirs = ['F:/Kaggle/seizures/train_1/train_1',
              'F:/Kaggle/seizures/train_2/train_2',
              'F:/Kaggle/seizures/train_3/train_3']

test_dirs = ['F:/Kaggle/seizures/test_1_new/test_1_new',
             'F:/Kaggle/seizures/test_2_new/test_2_new',
             'F:/Kaggle/seizures/test_3_new/test_3_new']

INTERICTAL = 0
PREICTAL = 1


data_files_train = get_file_names_and_classes(train_dirs)
data_files_test = get_file_names(test_dirs)
safe_df = pd.read_csv("F:/Kaggle/seizures/train_and_test_data_labels_safe.csv")

safe_files = [f for f in data_files_train if
        int(safe_df[safe_df.image == ntpath.basename(f['file'])].safe) == 1]

train_interictal = [f[0] for f in safe_files if f[1] == 0.0]
train_preictal = [f[0] for f in safe_files if f[1] == 1.0]

all_files = train_interictal + train_preictal

# Max_prop_zeros = .2: Must have greater than 80% complete data
# Things to think about changing:
# 1. Truncating on lines 94-95

first_pass = [2, 2, 2, 2, 4, 4, 4, 4, 8, 8, 10, 12, 14, 16]
high_res = [4, 4, 6, 6, 8, 8, 16, 16, 32, 32, 64, 64, 128, 128]
low_res = [2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 6, 6, 6, 6]

#generate_train(all_files, '2', 'haar', first_pass)
#generate_test(data_files_test, '2', 'haar', first_pass)
#
#generate_train(all_files, '3', 'coif1', first_pass)
#generate_test(data_files_test, '3', 'coif1', first_pass)
#
#generate_train(all_files, '4', 'coif1', low_res)
#generate_test(data_files_test, '4', 'coif1', low_res)
#
#generate_train(all_files, '5', 'coif1', high_res)
#generate_test(data_files_test, '5', 'coif1', high_res)

generate_train(all_files, '6', 'coif1', low_res)
generate_test(data_files_test, '6', 'coif1', low_res)


  # Data set 1
  #harr n_coefs: 15,15,30,59,118,235,469,938,1875,3750,7.5K,15K,30K,60K,120K
  #wavelet = 'haar'
  #n_buckets = [2, 2, 2, 2, 4, 4, 4, 4, 8, 8, 10, 12, 14, 16] # time resolution

  # Data set 2
  #wavelet = 'haar'
  #n_buckets = [4, 4, 4, 4, 8, 8, 8, 8, 16, 16, 20, 24, 28, 32] # time resolution

  # Data set 3
  #wavelet = 'haar'
  #n_buckets = [2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 6, 6, 8, 8] # time resolution

  # Data set 4
  #coif1 n_coefs: 19,19,34,63,122,239,473,942,1879,3754,7504,15004,...
  wavelet = 'coif1'
  n_buckets = [2, 2, 2, 2, 4, 4, 4, 4, 8, 8, 10, 12, 14, 16] # time resolution
        

explore_df = stack_dfs(all_files, max_prop_zeros = .2)
test_df = stack_dfs(data_files_test, max_prop_zeros = 1) 

explore_df.to_csv('c:/devl/kaggle/seizure-prediction/data/explore4.csv',
                  index = False)

test_df.to_csv('c:/devl/kaggle/seizure-prediction/data/test4.csv',
               index = False)

with PdfPages('c:/devl/plots/hist_vars.pdf') as pdf:
  for col in explore_df.columns[3:]:
    explore_df.hist(col)
    plt.title(col)
    pdf.savefig()
    plt.close()
     
    
#TODO: Histograms..Try to see impact of low data images
channel = 4
status = "preictal"

with PdfPages('c:/devl/plots/preictal_c4.pdf') as pdf:
  for i in range(50):
    mat = file_to_mat(explore_filenames.tolist()[i])
    mat = normalize_mat(mat)
    w_mat = wavelet_process_mat(mat, 8, 'haar')
    plt.plot(mat[channel, :])[0]
    plt.title(status + ", Image " + str(i) + " Channel: " + str(channel))
    pdf.savefig()
    plt.close()
    plt.plot(w_mat[channel, :])[0]
    plt.title(status + ", Processed Image " + str(i) +
              " Channel: " + str(channel))
    pdf.savefig()
    plt.close()

# Sampling

#np.random.seed(134)
#explore_interictal = np.random.choice(train_interictal, size = 50,
#                                      replace = False)
#explore_preictal = np.random.choice(train_preictal, size = 50,
#                                    replace = False)
#build_interictal = np.setdiff1d(train_interictal, explore_interictal)
#build_preictal = np.setdiff1d(train_preictal, explore_preictal)
#
#assert(len(train_interictal) == len(build_interictal) + len(explore_interictal))
#assert(len(train_preictal) == len(build_preictal) + len(explore_preictal))
#
#explore_filenames = np.append(explore_interictal, explore_preictal).tolist()
#explore_df = stack_dfs(explore_filenames, .2)

