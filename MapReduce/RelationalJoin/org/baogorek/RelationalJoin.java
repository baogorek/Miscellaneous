package org.baogorek;

import java.io.IOException;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.GenericOptionsParser;

public class RelationalJoin {

    public static class RelationalJoinMapper
        extends Mapper<Object, Text, Text, Text> {
        public void map(Object key, Text value, Context context)
            throws IOException, InterruptedException {
            String record = value.toString();
            record = record.substring(1, record.length() - 1);
            String[] fields = record.split(",");
            context.write(new Text(fields[1]), new Text(record));
        }
    }
    public static class RelationalJoinReducer
        extends Reducer<Text, Text, NullWritable, Text> {
        public void reduce(Text key, Iterable<Text> values,
                           Context context
                          ) throws IOException, InterruptedException {
             String order = "default";
             String lineItemConcat = "";
             for (Text val : values) {
                 String record = val.toString();
                 String[] fields = record.split(",");
                 if (fields[0].equals("\"order\"")) {
                     order = record;
                 } else {
                     lineItemConcat += record + ", ";
                 }
             }
             lineItemConcat = lineItemConcat.substring(0, lineItemConcat.length() - 2);
             String mergedRecord = "[" + order + ", " + lineItemConcat + "]";
             NullWritable nw = NullWritable.get();
             context.write(nw, new Text(mergedRecord));
        }
    }
   
    public static void main(String[] args) throws Exception {

        Job job = Job.getInstance();
        job.setJarByClass(RelationalJoin.class);
        job.setMapperClass(RelationalJoinMapper.class);
        job.setReducerClass(RelationalJoinReducer.class);
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);
        
        String basePath = "/hadoop-practice/relational-join/";
        FileInputFormat.addInputPath(job, new Path(basePath + "input"));
        FileOutputFormat.setOutputPath(job, new Path(basePath + "output"));

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
