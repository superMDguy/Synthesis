# -*- coding: utf-8 -*-
import subprocess
import pdb

import requests
from newspaper import Article
from newspaper.article import ArticleException
from segtok.segmenter import split_single, split_multi

SEARCH_URL = "https://www.googleapis.com/customsearch/v1?key=AIzaSyCmEb62AUBrhDxP_6p76Dp3e1b_jQ4157U&cx=001943372337957644989:uegyifhrqx0&q="


def search(term):
    items = requests.get(SEARCH_URL + term + " information").json()['items']
    urls = [item['link'] for item in items]
    return urls


def getArticles(term, sourceMap):
    urls = search(term)
    results = []
    for url in urls:
        if 'wikipedia.org' not in url:  # Don't reread wikipedia
            article = Article(url=url)
            article.download(html=getHTML(url))
            article.parse()
            if article.text:
                cleaned = cleanText(article.text, article.url, sourceMap)
                if cleaned:
                    results.append({'text': cleaned,
                                    'url': url})
    return results


def getHTML(url):
    return subprocess.check_output(["phantomjs", "getHTML.js", url]).decode('utf-8')


def cleanText(doc, url, sourceMap):
    paragraphs = doc.split('\n\n')
    totalSents = 0
    cleanParagraphs = []
    for paragraph in paragraphs:
        sentences = [sent for sent in split_single(paragraph)]
        cleanSentences = []
        if not len(sentences) < 2:  # Too short to be a paragraph
            totalSents += len(sentences)
            for sentence in sentences:
                # Make sure it's not too short, which would probably mean it's
                # a heading
                if not len(sentence.split(" ")) < 11:
                    # Remove extra whitespace
                    sentence = ' '.join(sentence.split())
                    sourceMap[sentence] = url
                    # Add clean sentence, along with source
                    cleanSentences.append(sentence)
            cleanParagraphs.append('  '.join(cleanSentences))
    if totalSents < 7:
        return False
    return cleanParagraphs


if __name__ == "__main__":
    url = "http://www.biography.com/people/barack-obama-12782369"
    article = Article(url)
    article.download(html=getHTML(url))
    article.parse()
    print(article.text)
