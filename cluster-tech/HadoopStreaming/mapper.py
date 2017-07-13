#!/usr/bin/env python
# From http://www.michael-noll.com/tutorials/writing-an-hadoop-mapreduce-program-in-python/
# Slightly modified for learning purposes

import sys

def read_input(file):
  for line in file:
    yield line.split()

def main():
  word_list_generator = read_input(sys.stdin) # create a generator
  for word_list in word_list_generator:
    for word in word_list:
      print '%s\t%d' % (word, 1)

if __name__ == "__main__":
  main()
