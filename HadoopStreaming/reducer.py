#!/usr/bin/env python
# From http://www.michael-noll.com/tutorials/writing-an-hadoop-mapreduce-program-in-python/
# Slightly modified for learning purposes

from operator import itemgetter
from itertools import groupby
import sys
import pdb

def read_mapper_output(mapper_output):
  for line in mapper_output:
    yield line.rstrip().split('\t', 1) # max of two elements w/ 1 split
    
def main():
  get_word_from_data_elem = itemgetter(0) # function to return first pos

  data = read_mapper_output(sys.stdin)
  keys_and_iterators = groupby(data, get_word_from_data_elem) 
  for key, data_elem_iterator in keys_and_iterators:
    count = sum(int(value) for key, value in data_elem_iterator)
    print "%s\t%d" % (key, count)

if __name__ == "__main__":
  main()
