
export HADOOP_CLASSPATH=$(/usr/bin/hadoop classpath)
javac -classpath $HADOOP_CLASSPATH -d InvertedIndex/ org/baogorek/InvertedIndex.java
ls -l InvertedIndex/org/baogorek
sleep 3
jar -cf InvertedIndex.jar -C InvertedIndex/ .
/usr/bin/hadoop jar InvertedIndex.jar org.baogorek.InvertedIndex
/usr/bin/hdfs dfs -cat  /hadoop-practice/inverted-index/output/*
/usr/bin/hdfs dfs -get  /hadoop-practice/inverted-index/output/part-r-00000 .
/usr/bin/hdfs dfs -rm -r  /hadoop-practice/inverted-index/output
rm -f InvertedIndex.jar
