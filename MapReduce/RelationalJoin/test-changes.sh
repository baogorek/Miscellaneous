rm -f  part*
export HADOOP_CLASSPATH=$(/usr/bin/hadoop classpath)
javac -classpath $HADOOP_CLASSPATH -d RelationalJoin org/baogorek/RelationalJoin.java
ls -l RelationalJoin/org/baogorek
sleep 3
jar -cf RelationalJoin.jar -C RelationalJoin/ .
/usr/bin/hadoop jar RelationalJoin.jar org.baogorek.RelationalJoin
/usr/bin/hdfs dfs -cat  /hadoop-practice/relational-join/output/*
/usr/bin/hdfs dfs -get  /hadoop-practice/relational-join/output/part-r-00000 .
/usr/bin/hdfs dfs -rm -r  /hadoop-practice/relational-join/output
rm -f RelationalJoin.jar
