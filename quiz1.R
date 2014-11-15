library(tm)

# load the data sets

newsSource <- DirSource(directory="final/en_US/", pattern="news", mode="text", ignore.case=FALSE);
newsCorpus <- VCorpus(newsSource);

blogsSource <- DirSource(directory="final/en_US/", pattern="blogs", mode="text", ignore.case=FALSE);
blogsCorpus <- VCorpus(blogsSource);

twitterSource <- DirSource(directory="final/en_US/", pattern="twitter", mode="text", ignore.case=FALSE);
twitterCorpus <- VCorpus(twitterSource);

rm(newsSource, blogsSource, twitterSource);

# look at the structure of the loaded text (i.e. character vector)

View(head(newsCorpus[[1]]$content));


###
# quiz 1 #
###

length(newsCorpus[[1]]$content);
length(blogsCorpus[[1]]$content);
length(twitterCorpus[[1]]$content);

ncharNews <- nchar(newsCorpus[[1]]$content);
ncharBlogs <- nchar(blogsCorpus[[1]]$content);
ncharTwitter <- nchar(twitterCorpus[[1]]$content);

max(ncharNews);
max(ncharBlogs);
max(ncharTwitter);

nLove <- length(grep("love", twitterCorpus[[1]]$content));
nHate <- length(grep("hate", twitterCorpus[[1]]$content));
nLove / nHate;

biostatsTweet <- twitterCorpus[[1]]$content[ grep("biostats", twitterCorpus[[1]]$content) ];
biostatsTweet

length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", 
                     twitterCorpus[[1]]$content));

