
library(tm);
library(plyr);

source("simpleTokenization.R");
source("multiGrams.R");


# define global variables for the minimum frequency of words and n-grams to be taken into account

MIN_WORD_FREQUENCY <- 10;
MIN_2GRAM_FREQUENCY <- 2;
MIN_3GRAM_FREQUENCY <- 2;
MIN_4GRAM_FREQUENCY <- 2;

# initialize variables to hold the distribution of n-grams and words

frequentWords <- NULL;
twoGramsDistr <- NULL;
threeGramsDistr <- NULL;
fourGramsDistr <- NULL;


# list the input files

filesList <- list.files(path="en_US/", pattern=".txt");

# filter some of the data sets...

filesList <- filesList[ -grep("blog", filesList)];
filesList <- filesList[ -grep("news", filesList)];


# loop through the data sets

for (i in 1:length(filesList))
{
    cat(paste("processing data set", i, "of", length(filesList), "\n"));
    
    # load the data sets
    
    dataSource <- DirSource(directory="en_US/", pattern=filesList[i], mode="text", ignore.case=FALSE);
    dataCorpus <- VCorpus(dataSource);
    rm(dataSource);
    
    # tokenize the input corpus
    
    cat("tokenizing text...\n");
    tokenizedText <- simpleTokenization(dataCorpus);
    
    
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
    
    wordsList <- unlist( strsplit(tokenizedText, split=" ") );
    wordsList <- wordsList[ wordsList != "" ];
    
    wordsTable <- table(wordsList);
    wordsTable <- sort(wordsTable, decreasing=TRUE);
    tempFrequentWords <- wordsTable[ wordsTable >= MIN_WORD_FREQUENCY];
    
    # # plot the distribution of the most frequent words
    
    # plot(tempFrequentWords[1:1000]/length(wordsList), log="y", xlab="words ordered by frequency", ylab="frequency", xaxt="n", 
    #     cex=0.5, main="relative frequencies of the 1000 most frequent words in a set of newspaper articles", pch=20);
    
    
    # convert the frequent words to a data frame and save it to the hard drive
    
    tempFrequentWords <- data.frame(word=rownames(tempFrequentWords), freq=tempFrequentWords, stringsAsFactors=FALSE);
    rownames(tempFrequentWords) <- NULL;    
    
    
    # calculate the relative cumulative sum of the word frequencies
    
    cumulativeSum <- cumsum(wordsTable);
    cumulativeSum <- cumulativeSum/length(wordsList);
    
    # calculate the number of words that cover 50 and 90 % of all the words
    
    cat("50% of words covered by number of words: \n");
    cat(paste(match(TRUE, cumulativeSum >= 0.5), "\n"));
    cat("90% of words covered by number of words: \n");
    cat(paste(match(TRUE, cumulativeSum >= 0.9), "\n"));

    
    # find ngrams...
    
    cat("find 2-grams\n");
    twoGrams <- multiGrams(tokenizedText,2);
    cat("find 3-grams\n");
    threeGrams <- multiGrams(tokenizedText,3);
    cat("find 4-grams\n");
    fourGrams <- multiGrams(tokenizedText,4);
    
    cat("number of 2-grams:\n");
    cat(paste(nrow(twoGrams), "\n"));
    cat("number of 3-grams:\n");
    cat(paste(nrow(threeGrams), "\n"));
    cat("number of 4-grams:\n");
    cat(paste(nrow(fourGrams), "\n"));
    
    # # filter only ngrams beginning with a "frequent word"
    # 
    # twoGramsFiltered <- twoGrams[ twoGrams[,1] %in% rownames(tempFrequentWords), ];
    # threeGramsFiltered <- threeGrams[ threeGrams[,1] %in% rownames(tempFrequentWords), ];
    
    # count the number of occurrences. to do so, first paste the multiple words together for faster counting...
    
    twoGramsPasted <- paste(twoGrams[,1], twoGrams[,2], sep=" ");
    threeGramsPasted <- paste(threeGrams[,1], threeGrams[,2], threeGrams[,3], sep=" ");
    fourGramsPasted <- paste(fourGrams[,1], fourGrams[,2], fourGrams[,3], fourGrams[,4], sep=" ");
    
    cat("find the distribution of 2-grams\n");
    twoGramsPasted <- table(twoGramsPasted);
    cat("find the distribution of 3-grams\n");
    threeGramsPasted <- table(threeGramsPasted);
    cat("find the distribution of 4-grams\n");
    fourGramsPasted <- table(fourGramsPasted);
    
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
    tempTwoGramsDistr <- data.frame(matrix(unlist(strsplit(rownames(twoGramsPasted), split=" ")), ncol=2, byrow=TRUE), 
                                stringsAsFactors=FALSE);
    tempThreeGramsDistr <- data.frame(matrix(unlist(strsplit(rownames(threeGramsPasted), split=" ")), ncol=3, byrow=TRUE), 
                                  stringsAsFactors=FALSE);
    tempFourGramsDistr <- data.frame(matrix(unlist(strsplit(rownames(fourGramsPasted), split=" ")), ncol=4, byrow=TRUE), 
                                 stringsAsFactors=FALSE);
    colnames(tempTwoGramsDistr) <- colnames(twoGrams);
    colnames(tempThreeGramsDistr) <- colnames(threeGrams);
    colnames(tempFourGramsDistr) <- colnames(fourGrams);
    
    tempTwoGramsDistr$freq <- twoGramsPasted;
    tempThreeGramsDistr$freq <- threeGramsPasted;
    tempFourGramsDistr$freq <- fourGramsPasted;
    
    # write the temporary data in the final variables
    
    if (length(frequentWords) == 0)
    {
        frequentWords <- tempFrequentWords;
        twoGramsDistr <- tempTwoGramsDistr;
        threeGramsDistr <- tempThreeGramsDistr;
        fourGramsDistr <- tempFourGramsDistr;
    }
    else
    {
        frequentWords <- rbind(frequentWords, tempFrequentWords);
        twoGramsDistr <- rbind(twoGramsDistr, tempTwoGramsDistr);
        threeGramsDistr <- rbind(threeGramsDistr, tempThreeGramsDistr);
        fourGramsDistr <- rbind(fourGramsDistr, tempFourGramsDistr);
    }
    
}
rm(i);


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
# currently less than 2 MB each. 

save(frequentWords, file="frequentWords.RData");
save(twoGramsDistr, file="twoGramsDistr.RData");
save(threeGramsDistr, file="threeGramsDistr.RData");
save(fourGramsDistr, file="fourGramsDistr.RData");



