
library(tm);
library(plyr);

source("simpleTokenization.R");
source("multiGrams.R");


# load the data sets

newsSource <- DirSource(directory="en_US/", pattern="news", mode="text", ignore.case=FALSE);
newsCorpus <- VCorpus(newsSource);

# blogsSource <- DirSource(directory="en_US/", pattern="blogs", mode="text", ignore.case=FALSE);
# blogsCorpus <- VCorpus(blogsSource);
# 
# twitterSource <- DirSource(directory="en_US/", pattern="twitter", mode="text", ignore.case=FALSE);
# twitterCorpus <- VCorpus(twitterSource);

rm(newsSource);
# rm(newsSource, blogsSource, twitterSource);

# look at the structure of the loaded text (i.e. character vector)

# View(head(newsCorpus[[1]]$content, n=20));


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

wordsTable <- table(wordsList);
wordsTable <- sort(wordsTable, decreasing=TRUE);
frequentWords <- wordsTable[ wordsTable >= 5];

# plot the distribution of the most frequent words

plot(frequentWords[1:1000]/length(wordsList), log="y", xlab="words ordered by frequency", ylab="frequency", xaxt="n", 
     cex=0.5, main="relative frequencies of the 1000 most frequent words in a set of newspaper articles", pch=20);


# convert the frequent words to a data frame and save it to the hard drive

frequentWords <- data.frame(word=rownames(frequentWords), freq=frequentWords, stringsAsFactors=FALSE);
rownames(frequentWords) <- NULL;
save(frequentWords, file="frequentWords.RData");



# calculate the relative cumulative sum of the word frequencies

cumulativeSum <- cumsum(wordsTable);
cumulativeSum <- cumulativeSum/length(wordsList);

# calculate the number of words that cover 50 and 90 % of all the words

match(TRUE, cumulativeSum >= 0.5);
match(TRUE, cumulativeSum >= 0.9);


# View(head(frequentWords, n=100));
# View(tail(frequentWords, n=100));

# wordsListUnique <- unique(wordsList);
# wordsListUnique <- sort(wordsListUnique);
# View(head(wordsListUnique, n=1000));


# find ngrams...

twoGrams <- multiGrams(tokenizedText,2);
threeGrams <- multiGrams(tokenizedText,3);

nrow(twoGrams);
nrow(threeGrams);

# # filter only ngrams beginning with a "frequent word"
# 
# twoGramsFiltered <- twoGrams[ twoGrams[,1] %in% rownames(frequentWords), ];
# threeGramsFiltered <- threeGrams[ threeGrams[,1] %in% rownames(frequentWords), ];

# count the number of occurrences. to do so, first paste the multiple words together for faster counting...

twoGramsPasted <- paste(twoGrams[,1], twoGrams[,2], sep=" ");
threeGramsPasted <- paste(threeGrams[,1], threeGrams[,2], threeGrams[,3], sep=" ");

twoGramsPasted <- table(twoGramsPasted);
threeGramsPasted <- table(threeGramsPasted);

# filter only multigrams with a minimum number of occurrences, e.g. at least twice

twoGramsPasted <- twoGramsPasted[ twoGramsPasted >= 2 ];
threeGramsPasted <- threeGramsPasted[ threeGramsPasted >= 2 ];

# order the multigrams by frequency

twoGramsPasted <- twoGramsPasted[ order(twoGramsPasted, decreasing=TRUE) ];
threeGramsPasted <- threeGramsPasted[ order(threeGramsPasted, decreasing=TRUE) ];

# convert them back to data frames

twoGramsDistr <- data.frame(matrix(unlist(strsplit(rownames(twoGramsPasted), split=" ")), ncol=2, byrow=TRUE), 
                            stringsAsFactors=FALSE);
threeGramsDistr <- data.frame(matrix(unlist(strsplit(rownames(threeGramsPasted), split=" ")), ncol=3, byrow=TRUE), 
                              stringsAsFactors=FALSE);
colnames(twoGramsDistr) <- colnames(twoGrams);
colnames(threeGramsDistr) <- colnames(threeGrams);

twoGramsDistr$freq <- twoGramsPasted;
threeGramsDistr$freq <- threeGramsPasted;



# plot the distribution of the most frequent two- and threegrams

plot(twoGramsDistr$freq[1:1000]/max(twoGramsDistr$freq), log="y", xlab="2-grams ordered by frequency", 
     ylab="frequency", xaxt="n", cex=0.5, pch=20, 
     main="relative frequencies of the 1000 most frequent 2-grams in a set of newspaper articles");

plot(threeGramsDistr$freq[1:1000]/max(threeGramsDistr$freq), log="y", xlab="3-grams ordered by frequency", 
     ylab="frequency", xaxt="n", cex=0.5, pch=20, 
     main="relative frequencies of the 1000 most frequent 3-grams in a set of newspaper articles");


# save the two- and threeGrams-distributions on the hard disk for faster loading in other R-scripts
# currently less than 2 MB each. 

save(twoGramsDistr, file="twoGramsDistr.RData");
save(threeGramsDistr, file="threeGramsDistr.RData");



