# script for trying to predict words with a simple model


source("simpleStringTokenization.R");


# load the 2- and 3-gram dristributions and the distribution of the most frequent single words 
# (all distributions ordered by decreasing frequency)

load("twoGramsDistr.RData");
load("threeGramsDistr.RData");
load("fourGramsDistr.RData");
load("frequentWords.RData");

# for the case of 3- and 4-grams, paste the first 2 / 3 words together to allow an easier search

threeGramsDistr[, 1] <- paste(threeGramsDistr[, 1], threeGramsDistr[, 2], sep=" ");
threeGramsDistr <- threeGramsDistr[, c(1,3,4)];
fourGramsDistr[, 1] <- paste(fourGramsDistr[, 1], fourGramsDistr[, 2], fourGramsDistr[, 3], sep=" ");
fourGramsDistr <- fourGramsDistr[, c(1,4,5)];


# define an input string (will be the argument of a function call at some point later on...)

INPUT_STRING <- "the same";

# tokenize the input string

tokenizedString <- simpleStringTokenization(INPUT_STRING);

# extract only the last element of the input string, split it by whitespaces and extract its length

inputString <- tokenizedString[ length(tokenizedString) ];
inputString <- unlist(strsplit(inputString, split=" "));
stringLength <- length(inputString);

# initialize a logical to hold the information whether a match has been found as FALSE
# and a character to hold the predicted word

matchFound <- FALSE;
predictedWord <- as.character(NULL);


# check for 4-grams

if (stringLength >= 3 & !matchFound)
{
    tempString <- paste(inputString[(stringLength-2):stringLength], collapse=" ");
    tempIndex <- match(tempString, fourGramsDistr[, 1]);
    
    if (!is.na(tempIndex))  # match found
    {
        predictedWord <- fourGramsDistr$word4[tempIndex];
        matchFound <- TRUE;
        cat("matching 4-gram found\n");
    }
    rm(tempString, tempIndex);
}


# check for 3-grams

if (stringLength >= 2 & !matchFound)
{
    tempString <- paste(inputString[(stringLength-1):stringLength], collapse=" ");
    tempIndex <- match(tempString, threeGramsDistr[, 1]);
    
    if (!is.na(tempIndex))  # match found
    {
        predictedWord <- threeGramsDistr$word3[tempIndex];
        matchFound <- TRUE;
        cat("matching 3-gram found\n");
    }
    rm(tempString, tempIndex);
}


# check for 2-grams

if (stringLength >= 1 & !matchFound)
{
    tempString <- inputString[stringLength];
    tempIndex <- match(tempString, twoGramsDistr[, 1]);
    
    if (!is.na(tempIndex))  # match found
    {
        predictedWord <- twoGramsDistr$word2[tempIndex];
        matchFound <- TRUE;
        cat("matching 2-gram found\n");
    }
    rm(tempString, tempIndex);
}


# if neither a matching 3- nor a 2-gram has been found, use the most frequent word in the data set as a guess

if (!matchFound)
{
    predictedWord <- frequentWords$word[1];
    cat("most frequent word chosen\n");
}

print(predictedWord);
