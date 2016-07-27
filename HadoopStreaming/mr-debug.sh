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

