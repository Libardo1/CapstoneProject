
library(tm)

source("simpleTokenization.R")


# load the data sets

newsSource <- DirSource(directory="en_US/", pattern="news", mode="text", ignore.case=FALSE);
newsCorpus <- VCorpus(newsSource);

blogsSource <- DirSource(directory="en_US/", pattern="blogs", mode="text", ignore.case=FALSE);
blogsCorpus <- VCorpus(blogsSource);

twitterSource <- DirSource(directory="en_US/", pattern="twitter", mode="text", ignore.case=FALSE);
twitterCorpus <- VCorpus(twitterSource);

rm(newsSource, blogsSource, twitterSource);

# look at the structure of the loaded text (i.e. character vector)

View(head(newsCorpus[[1]]$content, n=20));


# tokenize the input corpus

tokenizedText <- simpleTokenization(newsCorpus);


# profanity filtering

# since this algorithm (currently) uses newspaper articles as input, there is hardly any profane language. 
# this would probably look different with the twitter data set...
# just to test it, i'll remove all entries containig the word stem "idiot". 

toBeFiltered <- grep("idiot", tokenizedText);
if (length(toBeFiltered) > 0)
{
    tokenizedText <- tokenizedText[ -toBeFiltered ];
}
rm(toBeFiltered);



# get a list of words (to be used for guessing words e.g. at the start of a sentence or after words
# with no frequent combination)

wordsList <- unlist( strsplit(tokenizedText, split=" ") );
wordsList <- wordsList[ wordsList != "" ];

frequentWords <- table(wordsList);
frequentWords <- frequentWords[ frequentWords >= 5];
frequentWords <- sort(frequentWords, decreasing=TRUE);

View(head(frequentWords, n=100));
# View(tail(frequentWords, n=100));

wordsListUnique <- unique(wordsList);
wordsListUnique <- sort(wordsListUnique);
# View(head(wordsListUnique, n=1000));





