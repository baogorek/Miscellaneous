#!/usr/bin/env python

import sys
import getopt
import os

import numpy as np
import pandas as pd

from util import * # from util.py in same directory

def main(argv):
  try:
    opts, args = getopt.getopt(argv, "hi:o:", ["inputdir=", "outfile="])
  except getopt.GetoptError:
    print 'step1_processing.py -i <inputdir> -o <outputfile>'
    sys.exit(2)
  for opt, arg in opts:
    if opt == '-h':
      print 'step1_processing.py -i <inputdir> -o <outputfile>'
      sys.exit()
    elif opt in ("-i", "--inputdir"):
      inputdir = arg
    elif opt in ("-o", "--ofile"):
      outputfile = arg
  print("Welcome to the program")

  data_files_scoring = get_file_names_for_scoring(inputdir)

  # Create a temporary file with the input filenames - we may lose some later
  basenames = [os.path.basename(f) for f in data_files_scoring]

  data_files_df = pd.DataFrame(basenames, columns = ["File"])
  data_files_df.to_csv('input_filenames.csv', index = False)

  # Parameters: a few are also hiddedn within util.py
  low_res = [2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 6, 6, 6, 6]
  wavelet = 'coif1' 
  
  create_data_representation(data_files_scoring, outputfile, wavelet, low_res)

if __name__ == '__main__':
  main(sys.argv[1:])

