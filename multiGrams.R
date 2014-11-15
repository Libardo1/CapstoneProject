# function to extract multigrams (e.g. 2- or 3-grams)

multiGrams <- function(inputText, ngram)
{
    # filter elements with only one word (i.e. without whitespaces)
    
    inputText <- inputText[ grep(" ", inputText) ];
    
    # count the number of words in each string (i.e. in each element of the character vector)
    # by counting the whitespaces + 1
    
    numberOfWords <- sapply(gregexpr(" ", inputText), length) + 1;
    
    # extract strings with at least two / three words
    
    inputText <- inputText[ numberOfWords >= ngram ];
    numberOfWords <- numberOfWords[ numberOfWords >= ngram];
    
    # calculate the number of ngrams available in each element of inputText
    
    numberOfNgrams <- numberOfWords - ngram + 1;
    cumulativeNumber <- cumsum(numberOfNgrams);
    
    # create an array to hold all the ngrams
    
    ngramsArray <- array("", dim=c(sum(numberOfNgrams), ngram) );
    
    
    # loop through all the elements of inputText
    
    tempIndex <- 1;
    for (i in 1:length(inputText))
    {
        # split each element into single words words
        
        temp <- unlist( strsplit(inputText[i], split=" ") );
        tempN <- numberOfNgrams[i] - 1;
        
        for (j in 1:ngram)
        {
            ngramsArray[tempIndex:(tempIndex + tempN), j] <- temp[j:(tempN+j)];
        }
        rm(j);
        
        tempIndex <- cumulativeNumber[i] + 1;
    }
    rm(i, temp, tempIndex, tempN);
    
    # convert the result to a data frame and add column names
    
    ngramsArray <- data.frame(ngramsArray);
    colnames(ngramsArray) <- paste("word", 1:ngram, sep="");
    
    # return the result
    
    return(ngramsArray);
}
