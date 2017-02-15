from sklearn.linear_model import SGDClassifier
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer


def train(self):
    data = []
    target = []

    for title, paragraphs in self.sections.items():
        for sentence in paragraphs:
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
        paragraphs = doc['text'].split("\n\n")
        categories = self.classifier.predict(paragraphs)
        for i in range(len(paragraphs)):
            self.sections[categories[i]].append(paragraphs[i]) #Update the sections dictionary to add the new paragraph

