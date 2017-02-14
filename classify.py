from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.cluster import KMeans
from sklearn.metrics import adjusted_rand_score
from nltk.tokenize import sent_tokenize

sentences = sent_tokenize(article)

#vectorize the text i.e. convert the strings to numeric features

vectorizer = TfidfVectorizer(stop_words='english')
X = vectorizer.fit_transform(sentences)

#cluster documents

true_k = len(getHeadings(article))
model = KMeans(n_clusters=true_k, init='k-means++', max_iter=100, n_init=1)

result = model.fit_predict(X)

result_by_category = {}

for i in range (len(sentences)):
    print(str(sentences[i]) + "\t" + str(result[i]) + "\n\n")