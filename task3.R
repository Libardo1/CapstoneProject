# script for trying to predict words with a simple model


source("simpleStringTokenization.R");


# load the 2- and 3-gram dristributions (ordered by decreasing frequency)

load("twoGramsDistr.RData");
load("threeGramsDistr.RData");

# for the cast of threeGrams, past the first two words together to allow an easier search

threeGramsDistr[, 1] <- paste(threeGramsDistr[, 1], threeGramsDistr[, 2], sep=" ");
threeGramsDistr <- threeGramsDistr[, c(1,3,4)];


# define an input string (will be the argument of a function call at some point later on...)

INPUT_STRING <- "it must be Said 1234 , in order ";

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

# check for 3-grams

if (stringLength >= 2)
{
    tempString <- paste(inputString[(stringLength-1):stringLength], collapse=" ");
    tempIndex <- match(tempString, threeGramsDistr[, 1]);
    
    if (!is.na(tempIndex))  # match found
    {
        predictedWord <- threeGramsDistr$word3[tempIndex];
        matchFound <- TRUE;
    }
}
rm(tempString, tempIndex);



