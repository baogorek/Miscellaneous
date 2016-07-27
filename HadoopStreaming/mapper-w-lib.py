#!/usr/bin/env python

import sys
#import zipimport
# created requests.mod by zipping the requests folder inside the top-level
# project directory (has __init__.py in it) and renamed
#importer = zipimport.zipimporter('requests.mod')
#requests = importer.load_module('requests')

sys.path.append('myLibs')
import theano
import theano.tensor as T
from theano import function

def read_input(file):
  for line in file:
    yield line.split()

def main():
  x = T.scalar('x')
  y = T.scalar('y')
  z = x + y
  theano_fn = function([x, y], z)
  ans = theano_fn(1,2)
  #r = requests.post('http://httpbin.org/post', data = {'key':'value'})
  word_list_generator = read_input(sys.stdin) # create a generator
  for word_list in word_list_generator:
    for word in word_list:
      print '%s\t%d' % (word, 1)

if __name__ == "__main__":
  main()

