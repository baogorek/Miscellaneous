import pywt
import scipy
import scipy.io
import ntpath
from sklearn.preprocessing import normalize
import numpy as np
import pandas as pd
from os import path, listdir, walk
import re

def get_class_from_name(name):
    """
    Input: ".../1_1_1.mat" leads to Output: 1.0
    Input: ".../1_1_0.mat" leads to Output: 0.0
    """
    try:
        return int(name[-5])
    except:
        return -99

assert get_class_from_name('/train_1/1_1_0.mat') == 0
assert get_class_from_name('/train_1/1_1_1.mat') == 1

def get_testmat_from_name(name):
    """
    Input: ".../new_1_10.mat" leads to Output: 1 
    """
    end = ntpath.basename(name)
    m = re.match(".*(new_.*)$", end)
    try:
        return m.groups()[0]
    except:
        return '' 

assert get_testmat_from_name('/train_1/new_1_10.mat') == 'new_1_10.mat'

def get_user_from_name(name):
    """
    Input: ".../1_45_1.mat" leads to Output: 1 
    Input: ".../3_4_0.mat" leads to Output: 3 
    """
    m = re.match(".*(\d)_(\d+)_(\d).mat", name)
    try:
        return int(m.groups()[0]) 
    except:
        return -99 

assert get_user_from_name('/train_1/1_45_0.mat') == 1 
assert get_user_from_name('/train_1/3_4_1.mat') == 3 

def get_segment_from_name(name):
    """
    Input: ".../1_45_1.mat" leads to Output: 45 
    Input: ".../3_4_0.mat" leads to Output: 4 
    """
    m = re.match(".*(\d)_(\d+)_(\d).mat", name)
    try:
        return int(m.groups()[1]) 
    except:
        return -99 

assert get_segment_from_name('/train_1/1_45_0.mat') == 45 
assert get_segment_from_name('/train_1/3_4_1.mat') == 4 

def get_file_names(test_dirs):
  files = []
  for base_dir in test_dirs:
    files += [path.join(base_dir, f) for f in listdir(base_dir)]
  return(files)

def get_file_names_and_classes(train_dirs):
    ignored_files = ['.DS_Store', '1_45_1.mat']
    files = []
    for base_dir in train_dirs:
      files += [path.join(base_dir, f) for f in listdir(base_dir)
                if f not in ignored_files]
    return np.array(
        [
            (file, get_class_from_name(file))
               for file in files if file not in ignored_files
        ], dtype = [('file', '|S100'), ('class', 'float32')]

    )

def get_file_names_for_scoring(newdata_dir):
  # Traverses input directory and returns list of files
  data_files_scoring = []
  for root, subfolders, files in walk(newdata_dir):
    if len(files) > 0:
      data_files_scoring += [path.join(root, x) for x in files]
  return data_files_scoring

def file_to_mat(mat_filepath):
  print 'processing ' + mat_filepath
  mat_data = scipy.io.loadmat(mat_filepath)
  channels_data = mat_data['dataStruct']['data'][0][0].transpose()
  return(channels_data)

def normalize_mat(mat_data):
  mat_data = normalize(mat_data)
  # TODO: consider a gentler transformation besides this truncation
  mat_data[mat_data > 0.025] = 0.025
  mat_data[mat_data < -0.025] = -0.025
  return(mat_data)

def plot_channel(mat, channel = 0):
  plt.plot(mat[channel, :])
  plt.show()

def get_prop_zeros(mat_filename):
  mat = file_to_mat(mat_filename)
  return np.mean(mat == 0)

def create_wavelet_decomp(mat_filename, wavelet, n_buckets):
  mat = normalize_mat(file_to_mat(mat_filename))
  mat_data = []
  for channel in range(16):
    coefs = pywt.wavedec(mat[channel, :], wavelet, mode = 'sym', level = 14)
    for coefs_index in range(1, 15): 
      time_pieces = np.array_split(coefs[coefs_index], n_buckets[coefs_index - 1])
      t = 0
      for time_slice in time_pieces:
        t = t + 1
        zero_removed_t = time_slice[abs(time_slice) > 0.0]
        mat_data.append([mat_filename, channel, t, coefs_index,
                        len(zero_removed_t),
                        1000 * np.mean(abs(zero_removed_t))])
  return mat_data

def create_wide_df(filename, wavelet, n_buckets):
  mat2 = create_wavelet_decomp(filename, wavelet, n_buckets)
  mat_df = pd.DataFrame(mat2, columns = ['filename', 'channel', 't', 'scale',
                                         'n', 'energy'])
  
  mat_df['n_max'] = (mat_df.groupby(['filename', 'channel', 'scale'])['n']
                     .transform(max))
  mat_df = mat_df[(mat_df.n / mat_df.n_max > .30)] 

  # Pick the max energy timepoint within each scale
  mat_df['energy_max'] = (mat_df
                          .groupby(['filename', 'channel', 'scale'])['energy']
                          .transform(max))
  mat_df = mat_df[mat_df.energy == mat_df.energy_max]
  # In the case of energy_max ties, only keep one row per channel scale
  mat_df['n_rank'] = (mat_df
                      .groupby(['filename', 'channel', 'scale'])['n']
                      .cumcount(ascending = 0))
  mat_df = mat_df[mat_df.n_rank == 0]
 
  mat_df = mat_df.drop(['energy_max', 'n_rank', 'filename'], axis = 1) 
  var_names = "energy_" + mat_df.channel.map(str) + "_" + mat_df.scale.map(str)
  if "new" in filename:
    var_names = ["File"] + var_names.tolist()
    ids = [get_testmat_from_name(filename)]
  else:
    var_names = ["user", "segment", "preictal"] + var_names.tolist()

    ids = [get_user_from_name(filename), get_segment_from_name(filename),
           get_class_from_name(filename)]
  wide_df = pd.DataFrame([ids + mat_df.energy.tolist()], columns = var_names)
  return wide_df

def stack_dfs(filenames, max_prop_zeros, wavelet, n_buckets):

  dfs = []
  for filename in filenames:
    if get_prop_zeros(filename) < max_prop_zeros: 
      #TODO: filename to mat called twice. Opportunity to improve performance
      wide_df = create_wide_df(filename, wavelet, n_buckets)
      dfs.append(wide_df)
  wide_concat_df = pd.concat(dfs)
  return wide_concat_df 

def generate_train(all_files, str_ext, wavelet, n_buckets):
  explore_df = stack_dfs(all_files, max_prop_zeros = .6,
                         wavelet = wavelet, n_buckets = n_buckets)
  
  explore_dir = 'c:/devl/kaggle/seizure-prediction/data/'
  explore_filename = explore_dir + 'explore_' + str_ext + '.csv'
  explore_df.to_csv(explore_filename, index = False)
  
def generate_test(data_files_test, str_ext, wavelet, n_buckets):
  test_df = stack_dfs(data_files_test, max_prop_zeros = 1,
                      wavelet = wavelet, n_buckets = n_buckets)
  
  explore_dir = 'c:/devl/kaggle/seizure-prediction/data/'
  test_filename = explore_dir + 'test_' + str_ext + '.csv'
  test_df.to_csv(test_filename, index = False)

def create_data_representation(data_files, outfile, wavelet, n_buckets):
  test_df = stack_dfs(data_files, max_prop_zeros = 1,
                      wavelet = wavelet, n_buckets = n_buckets)
  test_df.to_csv(outfile, index = False)

def wavelet_process_mat(x, level = 8, wavelet = 'haar'):
  
  wavelet_coefs = []
  for channel in range(16):
    coefs = pywt.wavedec(x[channel, :], wavelet, mode = 'sym', level = level)
    wavelet_coefs.append(coefs[0])
  return np.array(wavelet_coefs, dtype = 'float32')

def get_X_from_files(files, level = 8, wavelet = 'haar'):
  """Creates data set for training from files"""
  data_matrices = []
  class_labels = []
  dropout_ct = 0
  for i, filename in enumerate(files):
    print('%{}: Loading file {}'.format(i, filename))
    try:
      mat = normalize_mat(filename)
      channels_data = process_mat(mat, level, wavelet)
      #channels_data = fourier_process_mat(filename)

    except ValueError as ex:
      print('Error loading MAT file {}: {}'.format(filename, str(ex)))
      continue

    prop_zeros = np.mean(channels_data.flatten() == 0)

    print('Proportion of 0s {}'.format(prop_zeros))
    if prop_zeros > .2:
      dropout_ct += 1
    else:
      data_matrices.append(normalize(channels_data).transpose())
      class_labels.append(get_class_from_name(filename))

  X = np.zeros((len(data_matrices), data_matrices[0].shape[0], 16),
               dtype = 'float32')
  y = np.array(class_labels)
  for i in range(len(data_matrices)):
    X[i, :, :] = data_matrices[i]
  print('dropout ct {}'.format(dropout_ct))
  return (X, y)
