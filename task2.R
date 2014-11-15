
library(tm);
library(plyr);

source("simpleTokenization.R");
source("multiGrams.R");

# define global variables for the maximum number of corpus elements and extracted strings to be used per data source

MAX_CORPUS_ELEMENTS <- 250000;
MAX_STRINGS <- 500000;

# define global variables for the minimum frequency of words and n-grams to be taken into account

MIN_WORD_FREQUENCY <- 10;
MIN_2GRAM_FREQUENCY <- 2;
MIN_3GRAM_FREQUENCY <- 2;
MIN_4GRAM_FREQUENCY <- 2;

# initialize variables to hold the lists of n-grams and words

wordsList <- as.character(NULL);
twoGramsPasted <- as.character(NULL);
threeGramsPasted <- as.character(NULL);
fourGramsPasted <- as.character(NULL);


# list the input files

filesList <- list.files(path="en_US/", pattern=".txt");

# filter some of the data sets...

# filesList <- filesList[ -grep("blog", filesList)];
# filesList <- filesList[ -grep("twitter", filesList)];


# loop through the data sets

for (i in 1:length(filesList))
{
    cat(paste("processing data set", i, "of", length(filesList), "\n"));
    cat(paste("current data set:", filesList[i], "\n"));
    
    # load the data sets
    
    dataSource <- DirSource(directory="en_US/", pattern=filesList[i], mode="text", ignore.case=FALSE);
    dataCorpus <- VCorpus(dataSource);
    rm(dataSource);
    
    # tokenize the input corpus, limiting the number of input elements to MAX_CORPUS_ELEMENTS
    
    cat("tokenizing text...\n");
    tokenizedText <- simpleTokenization(dataCorpus, maxElements=MAX_CORPUS_ELEMENTS);
    rm(dataCorpus);
    
    # limit the number of strings from the tokenized text (elements correspond roughly to (partial) sentences)
    
    tokenizedText <- tokenizedText[1:min(MAX_STRINGS, length(tokenizedText))];
    
    # profanity filtering
    
    # since this algorithm (currently) uses newspaper articles and blogs as input, there is little profane language. 
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
    
    tempWordsList <- unlist( strsplit(tokenizedText, split=" ") );
    tempWordsList <- tempWordsList[ tempWordsList != "" ];

    
    # find ngrams...
    
    cat("find 2-grams\n");
    twoGrams <- multiGrams(tokenizedText,2);
    cat("find 3-grams\n");
    threeGrams <- multiGrams(tokenizedText,3);
    cat("find 4-grams\n");
    fourGrams <- multiGrams(tokenizedText,4);
    
    rm(tokenizedText);

    
    # to count the number of occurrences more efficiently, paste the multiple words together (vectorization)...
    
    tempTwoGramsPasted <- paste(twoGrams[,1], twoGrams[,2], sep=" ");
    tempThreeGramsPasted <- paste(threeGrams[,1], threeGrams[,2], threeGrams[,3], sep=" ");
    tempFourGramsPasted <- paste(fourGrams[,1], fourGrams[,2], fourGrams[,3], fourGrams[,4], sep=" ");
    
    rm(twoGrams, threeGrams, fourGrams);
    
    # write the temporary data in the final variables
    
    wordsList <- c(wordsList, tempWordsList);
    twoGramsPasted <- c(twoGramsPasted, tempTwoGramsPasted);
    threeGramsPasted <- c(threeGramsPasted, tempThreeGramsPasted);
    fourGramsPasted <- c(fourGramsPasted, tempFourGramsPasted);
}
rm(i, tempWordsList, tempTwoGramsPasted, tempThreeGramsPasted, tempFourGramsPasted);


# find the distribution of the most frequent words...

cat("find the distribution of single words...\n");
wordsTable <- table(wordsList);
wordsTable <- sort(wordsTable, decreasing=TRUE);

# convert the words table to a data frame and save it to the hard drive

wordsTable <- data.frame(word=rownames(wordsTable), freq=wordsTable, stringsAsFactors=FALSE);
rownames(wordsTable) <- NULL;    
save(wordsTable, file="fullWordsTable.RData", compress="xz");


frequentWords <- wordsTable[ wordsTable$freq >= MIN_WORD_FREQUENCY, ];

# # plot the distribution of the most frequent words

# plot(frequentWords[1:1000]/length(wordsList), log="y", xlab="words ordered by frequency", ylab="frequency", xaxt="n", 
#     cex=0.5, main="relative frequencies of the 1000 most frequent words in a set of newspaper articles", pch=20);


# calculate the relative cumulative sum of the word frequencies

cumulativeSum <- cumsum(wordsTable);
cumulativeSum <- cumulativeSum/length(wordsList);
rm(wordsList);

# calculate the number of words that cover 50 and 90 % of all the words

cat("50% of words covered by number of words: \n");
cat(paste(match(TRUE, cumulativeSum >= 0.5), "\n"));
cat("90% of words covered by number of words: \n");
cat(paste(match(TRUE, cumulativeSum >= 0.9), "\n"));


# determine the distribution of the n-grams

cat("number of 2-grams:\n");
cat(paste(length(twoGramsPasted), "\n"));
cat("number of 3-grams:\n");
cat(paste(length(threeGramsPasted), "\n"));
cat("number of 4-grams:\n");
cat(paste(length(fourGramsPasted), "\n"));

cat("find the distribution of 2-grams\n");
twoGramsPasted <- table(twoGramsPasted);
cat("find the distribution of 3-grams\n");
threeGramsPasted <- table(threeGramsPasted);
cat("find the distribution of 4-grams\n");
fourGramsPasted <- table(fourGramsPasted);

# save the full distributions as pasted strings on the hard drive

save(twoGramsPasted, file="fullTwoGramsDistr.RData", compress="xz");
save(threeGramsPasted, file="fullThreeGramsDistr.RData", compress="xz");
save(fourGramsPasted, file="fullFourGramsDistr.RData", compress="xz");

# filter only multigrams with a minimum number of occurrences, e.g. at least twice

cat("filter and order the n-grams...\n");
twoGramsPasted <- twoGramsPasted[ twoGramsPasted >= MIN_2GRAM_FREQUENCY ];
threeGramsPasted <- threeGramsPasted[ threeGramsPasted >= MIN_3GRAM_FREQUENCY ];
fourGramsPasted <- fourGramsPasted[ fourGramsPasted >= MIN_4GRAM_FREQUENCY ];

# order the multigrams by frequency

twoGramsPasted <- twoGramsPasted[ order(twoGramsPasted, decreasing=TRUE) ];
threeGramsPasted <- threeGramsPasted[ order(threeGramsPasted, decreasing=TRUE) ];
fourGramsPasted <- fourGramsPasted[ order(fourGramsPasted, decreasing=TRUE) ];

# convert them back to data frames

cat("convert the n-grams to data frames\n");
twoGramsDistr <- data.frame(matrix(unlist(strsplit(rownames(twoGramsPasted), split=" ")), ncol=2, byrow=TRUE), 
                                stringsAsFactors=FALSE);
threeGramsDistr <- data.frame(matrix(unlist(strsplit(rownames(threeGramsPasted), split=" ")), ncol=3, byrow=TRUE), 
                                  stringsAsFactors=FALSE);
fourGramsDistr <- data.frame(matrix(unlist(strsplit(rownames(fourGramsPasted), split=" ")), ncol=4, byrow=TRUE), 
                                 stringsAsFactors=FALSE);
colnames(twoGramsDistr) <- paste("word", 1:2, sep="");
colnames(threeGramsDistr) <- paste("word", 1:3, sep="");
colnames(fourGramsDistr) <- paste("word", 1:4, sep="");

twoGramsDistr$freq <- twoGramsPasted;
threeGramsDistr$freq <- threeGramsPasted;
fourGramsDistr$freq <- fourGramsPasted;


# order the combined data by frequency

cat("order all the data...\n");
frequentWords <- frequentWords[ order(frequentWords$freq, decreasing=TRUE), ];
twoGramsDistr <- twoGramsDistr[ order(twoGramsDistr$freq, decreasing=TRUE), ];
threeGramsDistr <- threeGramsDistr[ order(threeGramsDistr$freq, decreasing=TRUE), ];
fourGramsDistr <- fourGramsDistr[ order(fourGramsDistr$freq, decreasing=TRUE), ];


# # plot the distribution of the most frequent two- and threegrams

# plot(twoGramsDistr$freq[1:1000]/max(twoGramsDistr$freq), log="y", xlab="2-grams ordered by frequency", 
#     ylab="frequency", xaxt="n", cex=0.5, pch=20, 
#     main="relative frequencies of the 1000 most frequent 2-grams in a set of newspaper articles");

# plot(threeGramsDistr$freq[1:1000]/max(threeGramsDistr$freq), log="y", xlab="3-grams ordered by frequency", 
#     ylab="frequency", xaxt="n", cex=0.5, pch=20, 
#     main="relative frequencies of the 1000 most frequent 3-grams in a set of newspaper articles");

# plot(fourGramsDistr$freq[1:1000]/max(fourGramsDistr$freq), log="y", xlab="4-grams ordered by frequency", 
#     ylab="frequency", xaxt="n", cex=0.5, pch=20, 
#     main="relative frequencies of the 1000 most frequent 4-grams in a set of newspaper articles");


# save the two- and threeGrams-distributions on the hard disk for faster loading in other R-scripts

save(frequentWords, file="frequentWords.RData");
save(twoGramsDistr, file="twoGramsDistr.RData");
save(threeGramsDistr, file="threeGramsDistr.RData");
save(fourGramsDistr, file="fourGramsDistr.RData");



