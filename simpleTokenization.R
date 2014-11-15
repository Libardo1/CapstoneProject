# function to tokenize an input text corpus (from the tm package)

simpleTokenization <- function(inputCorpus, maxElements=500000)
{
    # extract only the character vector of the corpus to increase computational speed and limit the number of elements
    
    inputText <- inputCorpus[[1]]$content[1:min(maxElements, length(inputCorpus[[1]]$content))];
    
    # remove all weird characters and numbers
    
    inputText <- iconv(inputText, "latin1", "ASCII", sub=" ");
    inputText <- gsub("[^[:alpha:][:space:][:punct:]]", "", inputText);
    
    # transform the text to lower case and replace multiple whitespaces by one
    
    inputText <- tolower(inputText);
    inputText <- gsub("( )+", " ", inputText);
    
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
