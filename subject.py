# -*- coding: utf-8 -*-

import re
from random import shuffle
import pdb

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
from search import getArticles
from segtok.segmenter import split_single, split_multi


def clean(string):
    return ' '.join(string.split())


class Subject:

    def __init__(self, subjectTitle, summaryLength=100, useWikipedia=True):
        self.subjectTitle = subjectTitle
        self.summaryLength = summaryLength
        self.useWikipedia = useWikipedia

        self.sourceMap = {}
        self.lengths = {}

    def build(self):
        self.wikiPage = wikipedia.page(wikipedia.search(self.subjectTitle)[0])
        self.sections = self.getSections()
        self.sources = getArticles(self.subjectTitle, self.sourceMap)
        self.classifier = self.train()

        self.classifySources()
        self.summarizeSections()
        return self

    def getSections(self):
        sectionsDict = {}
        content = self.wikiPage.content
        sections = re.findall('\n== (.*) ==\n', content)
        sections = [section for section in sections if section not in [
            "See also", "Bibliography", "Further reading", "References", "External links", "Notes", "Notes and references"]]
        for section in sections:
            start = content.index('== {0} =='.format(section))

            try:
                end = start + content[start:].index('\n== ')
            except ValueError:  # On last heading, no headings follow it
                end = -1

            # Remove all subheadings
            sectionContent = clean(
                re.sub('==* .* ==*', '', content[start:end]))
            self.lengths[section] = len(sectionContent)
            sentences = [sent for sent in split_single(
                sectionContent)]  # Split into sentences
            for sentence in sentences:
                # Add the source to the source map
                self.sourceMap[sentence] = self.wikiPage.url
            sectionsDict[section] = sentences
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
                                                   alpha=1e-5, n_iter=7)),
                             ])

        if not self.useWikipedia:
            for title, sentences in self.sections.items():
                # Clear sections if not using wikipedia
                self.sections[title] = []

        return text_clf.fit(data, target)

    def classifySources(self):
        for doc in self.sources:
            paragraphs = doc['text']
            categories = self.classifier.predict(paragraphs)
            for i in range(len(paragraphs)):
                # Update the sections dictionary to add the new sentence
                self.sections[categories[i]].append(paragraphs[i])

    def summarizeSections(self):
        for section, paragraphs in self.sections.items():
            # Set summary length of section to be proportional to complete
            # length of section
            print(section)
            summaryLength = round(
                self.lengths[section] / self.getTotalLength() * self.summaryLength)
            print(summaryLength)
            doc = '  '.join(paragraphs)
            parser = PlaintextParser.from_string(doc, Tokenizer('english'))
            stemmer = Stemmer('english')

            summarizer = Summarizer(stemmer)
            summarizer.stop_words = get_stop_words('english')
            summ = summarizer(parser.document, summaryLength)

            self.sections[section] = [str(sentence) for sentence in summ]
        print('done with summary')

    def getTotalLength(self):
        total = 0
        for section, length in self.lengths.items():
            total += length
        return total

    def __str__(self):
        return str(self.sections)

if __name__ == "__main__":
    subj = Subject("convolutional neural networks")
    subj.build()
    print(subj)
