
import nltk

examples = nltk.corpus.treebank.parsed_sents()[0:100]
tagged = nltk.corpus.treebank.tagged_sents()[0:100]

#choose a sentence by picking a value of k
k = 3
ex = examples[k]
sentence = " ".join(ex.leaves())
sentence

# Go from most specific to most general

# In order to create an NP-chunker, we will first define a chunk grammar, 
# consisting of rules that indicate how sentences should be chunked. 
# See http://www.eecis.udel.edu/~trnka/CISC889-11S/lectures/dongqing-chunking.pdf

patterns = """ 
        NP:     {<NNP><NN>*<NNS>}
                {<NNP>+<NN>}
                {<NN|NNP><POS>}
                {<DT>?<NNP>?<VBG><NN>+}
                {<JJ|QL>*<NNP>+}
                {<DT|PP\$>?<JJ|JJR|JJS|QL>*<NN>+<NNS>*} 
                {<DT|PP\$>?<JJ|JJR|JJS|QL>*<NNS>}
        VP:     {<VBD|VBZ><VBN>}
                {<RB><VBN>}
                {<VBD><IN>?}
                {<VBP><RP>?}
                {<VBZ>}
                {<VBN>}
                {<TO>?<VB>}
                {<VBG>}
        ADJP:   {}
        CDPP:   {<CD><NP><IN>?}
                {<CD><ADJP>}
                {<IN><DT>?<CD><NN>*<NNS>*<TO>?<CD>?}
        NP2:    {<NP><IN|TO>?<NP>}
        VP2:    {<VP><VP>+}
                {<VP><IN>?<NP>}
                {<VP><NP2>}
"""
NPChunker = nltk.RegexpParser(patterns)

result = NPChunker.parse(tagged[k])
result.draw()

