import subprocess
import pdb

import requests
from newspaper import Article
from newspaper.article import ArticleException

SEARCH_URL = "https://www.googleapis.com/customsearch/v1?key=AIzaSyCmEb62AUBrhDxP_6p76Dp3e1b_jQ4157U&cx=001943372337957644989:uegyifhrqx0&q="


def search(term):
    items = requests.get(SEARCH_URL + term).json()['items']
    urls = [item['link'] for item in items]
    return urls


def getArticles(term):
    urls = search(term)
    results = []
    for url in urls:
        if not 'wikipedia.org' in url:  # Don't look at wikipedia articles, that's already done
            article = Article(url=url)
            # pdb.set_trace()
            article.download(html=getHTML(url))
            article.parse()
            if article.text: #Make sure it was able to extract something
                results.append({'text': article.text,
                                'url': url})
    return results


def getHTML(url):
    return str(subprocess.check_output(["phantomjs", "getHTML.js", url]))

if __name__ == "__main__":
    url = "http://www.biography.com/people/barack-obama-12782369"
    article = Article(url)
    article.download(html=getHTML(url))
    article.parse()
    print(article.text)
