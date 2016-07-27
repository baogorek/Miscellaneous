chmod +x mapper.py
chmod +x reducer.py

echo "foo foo quux labs foo bar quux" > my_text

cat my_text | ./mapper.py | sort -k1,1 | ./reducer.py

hdfs dfs -mkdir /tutorial
hdfs dfs -mkdir /tutorial/input

hdfs dfs -copyFromLocal ./my_text /tutorial/input
hdfs dfs -cat /tutorial/input/*

# delete if it exists
hdfs dfs -rm -R /tutorial/output

# The real thing
hadoop jar /usr/lib/hadoop/hadoop-streaming.jar \
  -files ./mapper.py,./reducer.py  \
  -mapper ./mapper.py -reducer ./reducer.py \
  -input /tutorial/input/* -output /tutorial/output

### R section
chmod +x ./mapper-base.R
chmod +x ./reducer-base.R

hdfs dfs -rm -R /tutorial/output

# The real thing
hadoop jar /usr/lib/hadoop/hadoop-streaming.jar \
  -files ./mapper-base.R,./reducer-base.R \
  -mapper ./mapper-base.R \ -reducer ./reducer-base.R \
  -input /tutorial/input/* -output /tutorial/output

chmod +x ./mapper-iter.R
chmod +x ./reducer-iter.R

hdfs dfs -rm -R /tutorial/output

# With iterators package
# Build iterators package with R CMD INSTALL -l location --build pkg
# where pkg is the package that ends in tar.gz. I renamed the file to
# end with .tgz but I doubt that matters
hadoop jar /usr/lib/hadoop/hadoop-streaming.jar \
  -files ./mapper-iter.R,./reducer-base.R \
  -archives ./iterators_built.tgz#myLibs \
  -mapper ./mapper-iter.R \  -reducer ./reducer-base.R \ 
  -input /tutorial/input/* -output /tutorial/output

## For learning about the systems on the worker machines
hadoop jar /usr/lib/hadoop/hadoop-streaming.jar \
  -files ./dummy-mapper.sh,./dummy-reducer.sh \
  -archives ./iterators_built.tgz#myLibs \
  -mapper ./dummy-mapper.sh -reducer ./dummy-mapper.sh \ 
  -input /tutorial/input/* -output /tutorial/output

# Trying to send theano out to the nodes
chmod +x mapper-w-lib.py
# delete if it exists
hdfs dfs -rm -R /tutorial/output

# The real thing
hadoop jar /usr/lib/hadoop/hadoop-streaming.jar \
  -files ./mapper-w-lib.py,./reducer.py  \
  -archives ./theano.tar.gz#myLibs \
  -mapper ./mapper-w-lib.py -reducer ./reducer.py \
  -input /tutorial/input/* -output /tutorial/output

hdfs dfs -cat /tutorial/output/*


# finally found the log files on the worker node at:
# /mnt/var/log/hadoop-yarn/containers/application_1469620967087_0005/container_1469620967087_0005_01_000030
# This page pointed me in the right direction but really it was a lot of 
# grepping. Grepping for the library name got me to the right place

Traceback (most recent call last):
  File "/mnt2/yarn/usercache/hadoop/appcache/application_1469620967087_0005/container_1469620967087_0005_01_000030/././mapper-w-lib.py", line 11, in <module>
    import theano
  File "myLibs/theano/__init__.py", line 70, in <module>
    from theano.compile import (
  File "myLibs/theano/compile/__init__.py", line 10, in <module>
    from theano.compile.function_module import *
  File "myLibs/theano/compile/function_module.py", line 21, in <module>
    import theano.compile.mode
  File "myLibs/theano/compile/mode.py", line 12, in <module>
    import theano.gof.vm
  File "myLibs/theano/gof/vm.py", line 654, in <module>
    from . import lazylinker_c
  File "myLibs/theano/gof/lazylinker_c.py", line 42, in <module>
    location = os.path.join(config.compiledir, 'lazylinker_ext')
  File "myLibs/theano/configparser.py", line 324, in __get__
    self.__set__(cls, val_str)
  File "myLibs/theano/configparser.py", line 335, in __set__
    self.val = self.filter(val)
  File "myLibs/theano/configdefaults.py", line 1673, in filter_compiledir
    " '%s'. Check the permissions." % path)
ValueError: Unable to create the compiledir directory '/home/.theano/compiledir_Linux-4.4-amzn1.x86_64-x86_64-with-glibc2.2.5-x86_64-2.7.10-64'. Check the permissions.

