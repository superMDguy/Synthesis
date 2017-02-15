echo 'doing luhn'
sumy luhn --length=10 --url=http://en.wikipedia.org/wiki/Automatic_summarization >> 1.txt
echo 'doing edmundson'
sumy edmundson --length=10 --url=http://en.wikipedia.org/wiki/Automatic_summarization >> 2.txt
echo 'doing lsa'
sumy lsa --length=10 --url=http://en.wikipedia.org/wiki/Automatic_summarization >> 3.txt
echo 'doing lexrank'
sumy lex-rank --length=10 --url=http://en.wikipedia.org/wiki/Automatic_summarization >> 4.txt
echo 'doing textrank'
sumy text-rank --length=10 --url=http://en.wikipedia.org/wiki/Automatic_summarization >> 5.txt
echo 'doing sum-basic'
sumy sum-basic --length=10 --url=http://en.wikipedia.org/wiki/Automatic_summarization >> 6.txt
echo 'doing kl-sum'
sumy kl --length=10 --url=http://en.wikipedia.org/wiki/Automatic_summarization >> 7.txt
