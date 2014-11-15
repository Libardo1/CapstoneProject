# function to tokenize an input text string

library(tm);

simpleStringTokenization <- function(inputString)
{
    # remove all weird characters and numbers
    
    inputString <- iconv(inputString, "latin1", "ASCII", sub=" ");
    inputString <- gsub("[^[:alpha:][:space:][:punct:]]", "", inputString);
    
    # transform the text to lower case and replace multiple whitespaces by one
    
    inputString <- tolower(inputString);
    inputString <- gsub("( )+", " ", inputString);
    
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
