Exploration of Hadoop Streaming on Amazon EMR
============================================

The python files mapper.py and reducer are from [Michael Noll's blog](http://www.michael-noll.com/tutorials/writing-an-hadoop-mapreduce-program-in-python/).
I really wanted to understand the improved mapper and reducer files so I 
played around with the code, renamed the variables to my liking, etc.. One
thing I discovered is that the advanced versions still work in debug mode,
despite the article suggesting otherwise. I also thought I had seen it fail
 in the past,but nevertheless the first two lines in mr-debug.sh appear to work. 

On Amazon's EMR, Hadoop's keep programs like hadoop, hdfs, etc. are already in
the path. One thing I learned was not to run the binaries with the same names
from other parts of the filesystem. Also, don't forget to set up an SSH rule
on the Master node.
