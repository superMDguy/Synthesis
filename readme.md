
# Synthesis
*Automatic information extraction and classification*

## Motivation
A typical writing process involves gathering information from different sources, putting it together,
and adding in how it all fits together.  In almost all cases, this is done by the writer doing online research, finding the best
bits from other sources, and combining these extracts with their own unique take on the topic.  However, the research
portion can take a long time, which detracts from the more interesting and human part of writing.  So, I wondered if it'd be possible
to automate the sometimes boring task of extracting and categorizing the most important information on the subject, giving the
human more time to think about their unique outlook on the topic of interest.

It turns out that it is possible for a computer to do this, using modern NLP and machine learning techniques.  Though the
computerized version definitely can't achieve human level results, it could help a writer gather initial information from a variety
of sources, so they can gain basic knowledge about the field.

## How it Works
Here's the process the computer goes through when synthesizing the data:

### 1. Finds a wikipedia article on the subject using the `wikipedia` moudule
This gives the computer initial information, sorted into headings that can easily be extracted.

### 2. Trains an svm classifier on the wikipedia sections using `scikit-learn`
It goes through each wikipedia section, splits it into sentences, and then trains the svm classifier to identify under which header
a given sentence belongs.

### 3. Gets additional sources using google
Since the google search API is deprecated, I had to create a custom search engine, and then set it to search the web and return JSON.

### 4. Loads each additional source in phantomjs
While doing initial testing, I noticed that some websites didn't load properly, because they contained content injected by javascript.
Once the website loaded in phantomjs, the program `getHTML.js` prints out the rendered HTML.  The python script runs the node script,
and gets its output in order to get the rendered html of the website.

### 5. Extracts the important page content using the `newspaper` module
It loads the extracted html into an Article, and then parses it to get the main text of the article, while ignoring sidebars and other
unrelated content.

### 6. Classifies each sentence of the extracted article
It runs the trained classifier from step 3 on each sentence of the article, and adds it to the wikipedia sentences for that section.

### 7. Summarize it using the `sumy` module
It combines all sentences for each section, and then uses the `sumy` module to summarize it in a number of sentences, based on how many
sentences it has currently.

### 8. Done!
It outputs each section, along with each sentence of the section in a bulleted list.  I use markdown for the output, and convert it to
HTML to make things simpler.  Originally, the sentences were output in paragraph form, but when I read through it, the sentences
didn't "flow" well together.  This made the writing sound jerky and artificial.  Once I put it in a bulleted list, it felt much more
natural, like a typical list of notes.

## Extensions and Applications
When Google first came out with its search engine in the late 1990's, knowledge became much more easily accessible for students and
researchers all around.  I feel like this project could be a beginning to a new type of search engine; one that goes one level further
than what Google has already done.  This could be useful not just for writers, but for anyone who wants to get quick information from
a variety of sources.

Of course, the current implementation isn't very near to this ideal, and so a lot of work must be done.  I think their are three main
areas to focus on:

1. Source selection. I'd lke to develop some sort of algorithm to cull sources, so when I search for "pizza", it'll give me research
information on pizza, not the website for pizzahut.com.  I'm not exactly sure how I'd do this yet.javascript
2. Sentence classification.  Though I feel like the SVM classifier does a surprisingly job with limited data, it doesn't always get
it perfectly right.  I'm thinking of trying something like doc2vec so it can have some knowledge built in.
3. Summarization.  I'd like it to eliminate irrelevant sentences and sentences that need more context.
