echo "foo foo quux labs foo bar quux" > my_text

cat my_text | ./mapper.py | sort -k1,1 | ./reducer-noll.py

hdfs dfs -mkdir /tutorial
hdfs dfs -mkdir /tutorial/input

hdfs dfs -copyFromLocal ./my_text /tutorial/input
hdfs dfs -cat /tutorial/input/*

# delete if it exists
hdfs dfs -rm -R /tutorial/output

# The real thing
hadoop jar /usr/lib/hadoop/hadoop-streaming.jar \
  -file ./mapper.py    -mapper ./mapper.py \
  -file ./reducer.py   -reducer ./reducer.py \
  -input /tutorial/input/* -output /tutorial/output

