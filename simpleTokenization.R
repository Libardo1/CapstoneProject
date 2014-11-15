# function to tokenize an input text corpus

library(tm);

simpleTokenization <- function(inputCorpus, maxElements=500000)
{
    # limit the number of input elements from the corpus to maxElements
    
    inputCorpus[[1]]$content <- inputCorpus[[1]]$content[1:min(maxElements, length(inputCorpus[[1]]$content))];
    
    # remove all weird characters
    
    inputCorpus[[1]]$content <- iconv(inputCorpus[[1]]$content, "latin1", "ASCII", sub=" ");
    inputCorpus[[1]]$content <- gsub("[^[:alnum:][:space:][:punct:]]", "", inputCorpus[[1]]$content);
    
    
    # transform the news corpus for further processing
    
    inputCorpus <- tm_map(inputCorpus, stripWhitespace);
    inputCorpus <- tm_map(inputCorpus, content_transformer(tolower));
    inputCorpus[[1]]$content <- removeNumbers(inputCorpus[[1]]$content);
    
    
    # extract only the character vector of the corpus to increase computational speed
    
    inputText <- inputCorpus[[1]]$content;

    
    # split sentences (i.e. ".", ",", ";", "!", "?", "\"", " - ") into individual character elements
    
    inputText <- unlist( strsplit(inputText, split="\\.") );
    inputText <- unlist( strsplit(inputText, split="\\,") );
    inputText <- unlist( strsplit(inputText, split="\\!") );
    inputText <- unlist( strsplit(inputText, split="\\?") );
    inputText <- unlist( strsplit(inputText, split="\\;") );
    inputText <- unlist( strsplit(inputText, split="\"") );
    inputText <- unlist( strsplit(inputText, split=" (-)+ ") );
    
    
    # remove leading and ending whitespaces
    
    inputText <- gsub("^( )+", "", inputText);
    inputText <- gsub("(( )+)$", "", inputText);
    
    
    # remove punctuation apart from - and '
    
    inputText <- gsub("([-\'])|[[:punct:]]", "\\1", inputText);
    
    
    # remove leading and ending apostrophes and - in words
    
    inputText <- gsub(" (['-])+", "", inputText);
    inputText <- gsub("['-]+ ", "", inputText);
    inputText <- gsub("^(['-])+", "", inputText);
    inputText <- gsub("['-]+$", "", inputText);
    
    # remove empty elements
    
    inputText <- inputText[ inputText != "( )+" ];
    inputText <- inputText[ inputText != "" ];
    
    # reduce multiple whitespaces to one
    
    inputText <- gsub("( ){2,}", " ", inputText);
    
    # remove leading and ending whitespaces
    
    inputText <- gsub("^( )+", "", inputText);
    inputText <- gsub("(( )+)$", "", inputText);
    
    # return the tokenized input Text
    
    return (inputText);
}
