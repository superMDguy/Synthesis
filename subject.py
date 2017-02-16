# -*- coding: utf-8 -*-
import re
from random import shuffle

import wikipedia
import sumy
from sklearn.linear_model import SGDClassifier
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
from sumy.parsers.plaintext import PlaintextParser
from sumy.nlp.tokenizers import Tokenizer
from sumy.summarizers.lex_rank import LexRankSummarizer as Summarizer
from sumy.nlp.stemmers import Stemmer
from sumy.utils import get_stop_words
from nltk.tokenize import sent_tokenize

from search import getArticles

def clean(string):
    return ' '.join(string.split())

class Subject:
    def __init__(self, subjectTitle, summaryLength=100):
        self.subjectTitle = subjectTitle
        self.summaryLength = summaryLength

        self.wikiPage = wikipedia.page(wikipedia.search(self.subjectTitle)[0])
        self.sections = self.getSections()
        self.sources = getArticles(subjectTitle)
        self.classifier = self.train()

        self.classifySources()
        self.summarizeSections()

    def getSections(self):
        sectionsDict = {}
        content = self.wikiPage.content
        sections = re.findall('\n== (.*) ==\n', content)
        sections = [section for section in sections if not section in ["See also", "Bibliography", "Further reading", "References", "External links"]]
        for section in sections:
            start = content.index('== {0} =='.format(section))

            try:
                end = start + content[start:].index('\n== ')
            except ValueError: #On last heading, no headings follow it
                end = -1

            sectionContent = clean(re.sub('==* .* ==*', '', content[start:end])) #Remove all subheadings
            sectionsDict[section] = sent_tokenize(sectionContent) #Split into sentences
        return sectionsDict

    def train(self):
        data = []
        target = []

        for title, sentences in self.sections.items():
            for sentence in sentences:
                data.append(str(sentence))
                target.append(title)

        text_clf = Pipeline([('vect', CountVectorizer()),
                            ('tfidf', TfidfTransformer()),
                            ('clf', SGDClassifier(loss='hinge', penalty='l2',
                                                alpha=1e-3, n_iter=5, random_state=42)),
                            ])
        return text_clf.fit(data, target)

    def classifySources(self):
        for doc in self.sources:
            paragraphs = doc['text']
            categories = self.classifier.predict(paragraphs)
            for i in range(len(paragraphs)):
                self.sections[categories[i]].append(paragraphs[i]) #Update the sections dictionary to add the new sentence

    def summarizeSections(self):
        summaryLength = round((len(self.sections)/self.getTotalLength())*self.summaryLength) #Set summary length of section to be proportional to complete length of section
        # for section, sentences in self.sections.items():
        #     doc = '\n\n'.join(sentences)
        #     parser = PlaintextParser.from_string(doc, Tokenizer('english'))
        #     stemmer = Stemmer('english')

        #     summarizer = Summarizer(stemmer)
        #     summarizer.stop_words = get_stop_words('english')
        #     summ = summarizer(parser.document, summaryLength)

        #     self.sections[section] = [str(sentence) + " ({0})".format(self.getSource(sentence)) for sentence in summ]

    def getSource(self, sentence):
        for source in self.sources:
            if str(sentence) in source['text']:
                return '[source]({0})'.format(source['url'])
        return '[source]({0})'.format(self.wikiPage.url) #The wikipedia page isn't included with the rest of the sources.

    def getTotalLength(self):
        total = 0
        for section in self.sections:
            total += len(section)
        return total

    def __str__(self):
        '''Outputs final document as markdown'''
        out = "# " + self.wikiPage.title
        for section,sentences in self.sections.items():
            out += "\n\n## " + section + "\n"
            for sentence in sentences:
                out += "- " + sentence + "\n"
        return out

if __name__ == "__main__":
    subj = Subject("convolutional neural networks")
    print(subj)