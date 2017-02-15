
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
It goes through each wikipedia section, splits it into paragraphs, and then trains the svm classifier to identify under which header
a given paragraph belongs.

### 3. Gets additional sources using google
Since the google search API is deprecated, I had to create a custom search engine, and then set it to search the web and return JSON.

### 4. Loads each additional source in phantomjs
While doing initial testing, I noticed that some websites didn't load properly, because they contained content injected by javascript.
Once the website loaded in phantomjs, the program `getHTML.js` prints out the rendered HTML.  The python script runs the node script,
and gets its output in order to get the rendered html of the website.

### 5. Extracts the important page content using the `newspaper` module
It loads the extracted html into an Article, and then parses it to get the main text of the article, while ignoring sidebars and other
unrelated content.

### 6. Classifies each paragraph of the extracted article using the already trained classifier.
