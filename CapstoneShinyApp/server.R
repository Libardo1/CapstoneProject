# code file for the server code file for the shiny app

library(shiny)


# function to tokenize an input text string

simpleStringTokenization <- function(inputString)
{
    # remove all weird characters and numbers
    
    inputString <- iconv(inputString, "latin1", "ASCII", sub=" ");
    inputString <- gsub("[^[:alpha:][:space:][:punct:]]", "", inputString);
    
    # transform the text to lower case and replace multiple whitespaces by one
    
    inputString <- tolower(inputString);
    inputString <- gsub("( )+", " ", inputString);
    
    # return empty input strings
    
    if (nchar(inputString) == 0) return("");
    
    # split sentences (i.e. ".", ",", ";", "!", "?", "\"", " - ") into individual character elements
    
    inputString <- unlist( strsplit(inputString, split="\\.") );
    inputString <- unlist( strsplit(inputString, split="\\,") );
    inputString <- unlist( strsplit(inputString, split="\\!") );
    inputString <- unlist( strsplit(inputString, split="\\?") );
    inputString <- unlist( strsplit(inputString, split="\\;") );
    inputString <- unlist( strsplit(inputString, split="\"") );
    inputString <- unlist( strsplit(inputString, split=" (-)+ ") );
    
    # remove punctuation apart from - and '
    
    inputString <- gsub("([-\'])|[[:punct:]]", "\\1", inputString);
    
    # remove leading and ending apostrophes and - in words
    
    inputString <- gsub(" (['-])+", "", inputString);
    inputString <- gsub("['-]+ ", "", inputString);
    inputString <- gsub("^(['-])+", "", inputString);
    inputString <- gsub("['-]+$", "", inputString);
    
    # reduce multiple whitespaces to one
    
    inputString <- gsub("( ){2,}", " ", inputString);
    
    # remove leading and ending whitespaces
    
    inputString <- gsub("^( )+", "", inputString);
    inputString <- gsub("(( )+)$", "", inputString);
    
    # return the tokenized input Text
    
    return (inputString);
}


# function to predict the next word for a given input character string

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

predictNextWord <- function(inputString)
{    
    # tokenize the input string
    
    inputString <- simpleStringTokenization(inputString);
    
    # extract only the last element of the input string, split it by whitespaces and extract its length
    
    inputString <- inputString[ length(inputString) ];
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
            cat("matching 4-gram found:\n");
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
            cat("matching 3-gram found:\n");
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
            cat("matching 2-gram found:\n");
        }
        rm(tempString, tempIndex);
    }
    
    # if neither a matching 3- nor a 2-gram has been found, use the most frequent word in the data set as a guess
    
    if (!matchFound)
    {
        predictedWord <- frequentWords$word[1];
        cat("no n-gram found, most frequent word chosen:\n");
    }
    
    return(predictedWord);
}


shinyServer(
    function(input, output) {
        output$prediction <- renderPrint({
            temp <- simpleStringTokenization(input$inputString);
            temp <- predictNextWord(temp);
            cat("\n\t");
            cat(temp);
            })
    }
)
