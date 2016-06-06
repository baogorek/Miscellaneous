package org.baogorek;

import java.io.IOException;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.StringTokenizer;
import java.util.HashSet;
import java.util.Set;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.GenericOptionsParser;

public class InvertedIndex {
    public static Pattern booksPattern = Pattern.compile("\\[\"(.*)\", \"(.*)\"\\]");

    public static class InvertedIndexMapper
         extends Mapper<Object, Text, Text, Text> {
        public void map(Object key, Text value, Context context
                       ) throws IOException, InterruptedException{
            // OPEN QUESTION: what is KEYIN (key) here? The str rep is an integer
            Matcher matcher = booksPattern.matcher(value.toString());
            Text title = new Text();
            Text token = new Text();
            String content = "";
            while (matcher.find()) {
                // You "set" a Text data type to a string value
                title.set(matcher.group(1));
                content = matcher.group(2);
            }
            StringTokenizer tokenizer = new StringTokenizer(content); 
            while (tokenizer.hasMoreTokens()) {
                token.set(tokenizer.nextToken());
                context.write(token, title);
            }
        }
    }

    public static class InvertedIndexReducer
        extends Reducer<Text, Text, Text, Text> {
        public void reduce(Text key, Iterable<Text> values,
                           Context context
                          ) throws IOException, InterruptedException {
        Text wordConcat = new Text();
        Text titleConcat = new Text();

        wordConcat.set("[\"" + key.toString() + "\", ");
        Set<String> set = new HashSet<String>();
        for (Text val : values) {
            set.add("\"" + val + "\"");
        }
        List<String> titleList = new ArrayList<String>();
        titleList.addAll(set);
        java.util.Collections.sort(titleList);
        String outputString = titleList.toString() + "]";
        titleConcat.set(outputString);
        context.write(wordConcat, titleConcat);
        }
    }

    public static void main(String[] args) throws Exception {
  
        Job job = Job.getInstance();
        job.setJarByClass(InvertedIndex.class);
        job.setMapperClass(InvertedIndexMapper.class);
        job.setReducerClass(InvertedIndexReducer.class);

        // Without these two lines, the job would finish but there would be no output
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);
 
        FileInputFormat.addInputPath(job, new Path("/hadoop-practice/inverted-index/input"));
        FileOutputFormat.setOutputPath(job, new Path("/hadoop-practice/inverted-index/output"));
       System.exit(job.waitForCompletion(true) ? 0 : 1); 
    }
}
  
