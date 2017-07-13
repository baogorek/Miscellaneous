import xml.etree.ElementTree as ET
from collections import namedtuple

class Connection():
    def __init__(self):
        self.first_name = ""
        self.last_name = ""
        self.connection_ct = -999
        self.pos_summary = ""
        self.start_year = -999
        self.start_month = -999
        self.tokens = []
        self.lexical_diversity = 0.00
        self.freq_dist = {}

connections = []

iter_parsed = ET.iterparse('/home/user/eclipse_space/Social_Media/Ben_connections.xml')

# I'm aware there is a better way
for item in iter_parsed:
    element = item[1]
    
    if element.tag == "person":
        connection = Connection()
        for x in element.getchildren():
            if x.tag == "first-name":
                connection.first_name = x.text
            if x.tag == "last-name":
                connection.last_name = x.text
            if x.tag == "num-connections":
                connection.connection_ct = int(x.text)
            # TODO: I should have queried for this using Xpath           
            if x.tag == "positions":
                for y in x.getchildren():
                    if y.tag == "position":
                        for z in y.getchildren():
                            if z.tag == "summary":
                                connection.pos_summary = z.text
                            if z.tag == "start-date":
                                for w in z.getchildren():
                                    if w.tag == "year":
                                        connection.start_year = int(w.text)
                                    if w.tag == "month":
                                        connection.start_month = int(w.text)
                                
        connections.append(connection)
        

for i in range(len(connections)):
    if connections[i].pos_summary not in [None, ""]:
        print "connection", str(i)
        print connections[i].pos_summary

import nltk
from nltk.stem import PorterStemmer
import re

stemmer = PorterStemmer()
stopwords = nltk.corpus.stopwords.words('english')

c = connections[131]
print c.first_name
print c.last_name
print c.connection_ct
print c.start_month
print c.start_year
print str(c.pos_summary)

all_words = []
for c in connections:
    summary = re.sub("[\\t]*[\\n]*\s*\\xe2\\x80\\xa2[\\t]*[\\n]*\s*", ". ", str(c.pos_summary))
    summary = re.sub("\\xe2\\x80\\x99[s]", "", summary) 
    summary = re.sub("-|\+", "", summary)
    summary = re.sub("\d+", "num", summary)
    sentences = [sent for sent in nltk.sent_tokenize(summary) if len(sent) > 2]

    c.tokens = [stemmer.stem(word.lower()) for sent in sentences for word in nltk.word_tokenize(sent) \
                                     if word.lower() not in stopwords and len(word) > 2]
    all_words += c.tokens
    c.freq_dist = nltk.FreqDist(c.tokens)
    if len(c.tokens) > 0:
        c.lexical_diversity = float(len(set(c.tokens))) / float(len(c.tokens))
        


freq_dist = nltk.FreqDist(all_words)
for token in freq_dist.keys()[:25]:
    print token, freq_dist[token]

markers = freq_dist.keys()[:10]


out = open('/home/user/eclipse_space/Social_Media/con_set.txt', 'wb')
name_str = "\t".join(['first_name', 'last_name', 'start_year', 'start_month', 'connection_ct', 'lexical_diversity', 'n_tokens'] + markers) + "\n"
out.write(name_str)
    
for c in connections:
    
    out.write(c.first_name + '\t'+ c.last_name + '\t' +  str(c.start_year) + '\t' + str(c.start_month) + '\t' + str(c.connection_ct) + '\t' +  str(c.lexical_diversity) + '\t' + str(len(c.tokens)))
    for token in markers:
        out.write('\t' + str(c.freq_dist[token]))
    out.write('\n')
        
out.close()
      
        
