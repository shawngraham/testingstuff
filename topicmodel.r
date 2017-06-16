# some packages we need; you only ever need install once
install.packages("mallet")
library("mallet")
install.packages('RCurl')
library(RCurl)

# Importing data directly from the web----------

#Melodee Beals has been using TEI to markup newspaper articles, creating the Colonial 
#Newspapers Database (which she shared on github). We then used Github Pages and an
#XLST stylesheet to convert that database into a table of comma-separated values 
#https://raw.githubusercontent.com/shawngraham/exercise/gh-pages/CND.csv. 
# We are now going to topic model the text of those newspaper articles, to see what
# patterns of discourse may lie within.

# Now we want to tell R Studio to grab our data from our github page. 
# The thing is, R Studio can easily grab materials from websites where 
# the url is http; but when it is https (as it is with github), things 
# get a bit more fussy. So what we do is use a special package to grab the 
# data, and then shove it into a variable that we can then tease apart for 
# our analysis.

x <- getURL("https://raw.githubusercontent.com/shawngraham/exercise/gh-pages/CND.csv", .opts = list(ssl.verifypeer = FALSE))
documents <- read.csv(text = x, col.names=c("Article_ID", "Newspaper Title", "Newspaper City", "Newspaper Province", "Newspaper Country", "Year", "Month", "Day", "Article Type", "Text", "Keywords"), colClasses=rep("character", 3), sep=",", quote="")

# Remember how we used ‘curl’ in one of our scripts in an earlier module to grab 
# data from the Canadiana.org api? RCurl is its doppleganger within R. So, we create 
# a variable called ‘x’ and we pass to it the Colonial Newspaper Database as csv. 
# Then, we read the contents of that csv file and tell R what the structure of the 
# data is. All of this gets passed to a variable called ‘documents’. Incidentally, 
# if we then wanted to look only at the keywords in this table, we would do that by 
# querying ‘documents$Keywords’. The $ is quite handy!

# Now, let’s take a look at our data.

counts <- table(documents$Newspaper.City)
# we tell R to make a new variable called 'counts', by grabbing the information in the table 'documents' from the column 'newspaper city'

barplot(counts, main="Cities", xlab="Number of Articles")
# Clearly, we’re getting an Edinburgh/Glasgow perspective on things. 
# And somewhere in our data, there’s a mispelled ‘Edinbugh’. Do you see 
# any other error(s) in the plot? How would you correct it(them)?

years <- table(documents$Year)
barplot(years, main="Publication Year", xlab="Year", ylab="Number of Articles")

#There’s a lot of material in 1789, another peak around 1819, againg in the late 1830s. 
# We can ask ourselves now: is this an artefact of the data, or of our collection methods?
# This would be a question a reviewer would want answers to. Let’s assume for now that 
# these two plots are ‘true’ - that, for whatever reasons, only Edinburgh and Glasgow were
# concerned with these colonial reports, and that they were particulary interested during
# those three periods. This is already an interesting question that we as historians would
# want to explore. Try making some more visualizations like this of other aspects of the 
# data. What other patterns do you see that are worth investigating? 

# This quick distant look now necessitates some close reading - and back again! But first...

# In the line below, note that there is a file called 'en.txt' that it wants to load up.
# To create that file, click on the 'new file' icon in the tool ribbon and select
# new text file. This will open a blank file in the edit window here.
# copy and paste the list of words at http://www.matthewjockers.net/macroanalysisbook/expanded-stopwords-list/
# into that blank file and save it as en.txt

mallet.instances <- mallet.import(documents$Article_ID, documents$Text, "en.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

#That line above passes the article ID and the text of our newspaper articles to the
# Mallet routine.  The stopwords list is generic; it might need to be curated to take into
# account the pecularities of your data. You might want to create your own, one for each
# project given the particulars of your project. Note that Jockers compiled hist stoplist 
# for his research in literary history of the 19th century. Your mileage may vary! 
# Finally, the last bit after ‘token.regexp’ applies a regular expression against our
# newspaper articles, cleaning them up.

#-----------------------------
# reading data from a directory
#-----------------------------
# This is an alternative way of ingesting documents for topic modeling.
# Earlier, you learned how to use wget and some other scripts to download full text documents
# from Canadiana.org as well as from the Provincial Archives in Quebec (the Shawville Equity).
# the code below loads those documents into Mallet, after which you can proceed to 
# build a topic model. In the command line, cd into your folder that has your downloaded
# materials. Type $ pwd to get the full path, then copy and paste it into the line below between
# the " marks.

documents <- mallet.read.dir("/home/shawngraham/equity")

mallet.instances <- mallet.import(documents$id, documents$text, "en.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#---------------

#set the number of desired topics
num.topics <- 20
topic.model <- MalletLDA(num.topics)

#Now we’ve told Mallet how many topics to search for; this is a number you’d want to
# fiddle with, to find the ‘best’ number of topics. The next line creates a variable 
# ‘topic.model’ which will eventually be filled by Mallet using the LDA approach, for
# 50 topics. Let’s get some info on our topic model, on our distribution of words in 
# these newspaper articles.

topic.model$loadDocuments(mallet.instances)
## Get the vocabulary, and some statistics about word frequencies.
## These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)

# write our output to our project folder; that way you can bring it into other programs if you want
write.csv(word.freqs, "word-freqs.csv" )

# if we take a look at word frequencies right now, before we generate the model, 
# we can see if there are words that would be better added to our stopword list. 
# The final line writes these word frequencies to a new csv file. Do you see how you 
# might create a bar plot of word frequencies?

# Now we do the heavy lifting: generating a topic model. Some of the comments below are very 
# technical; just make sure to run each line of code!

## Optimize hyperparameters every 20 iterations,
## after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)
## Now train a model. Note that hyperparameter optimization is on, by default.
## We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(1000)
## Run through a few iterations where we pick the best topic for each token,
## rather than sampling from the posterior distribution.

topic.model$maximize(10)
## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

# Let’s look at some of our topics.
## What are the top words in topic 7?
## Notice that R indexes from 1, so this will be the topic that mallet called topic 6.
mallet.top.words(topic.model, topic.words[7,])

# Now we’ll write the distribution of the topics by document (ie newspaper article) 
# to a csv file that we could explore/visualize with other tools. Then, we’ll take a 
# look at the key words describing each topic.

topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
write.csv(topic.docs, "topics-docs.csv" ) 
# that file enables you to see what topics are most present in what issues/documents

## Get a vector containing short names for the topics
topics.labels <- rep("", num.topics)
for (topic in 1:num.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" ")
# have a look at keywords for each topic
topics.labels

write.csv(topics.labels, "topics-labels.csv")

# Some interesting patterns suggest themselves already! 
# But a list of words doesn’t capture the relative importance of particular words 
# in particular topics. A word might appear in more than one topic, for instance, 
# but really dominate one rather than the other. Word-clouds, that much-maligned 
# technique, is actually rather useful in this regard. Let’s visualize these topic words.

### histogram
plot(hclust(dist(topic.words)), labels=topics.labels)

#The plot is a bit crowded; in RStudio you can open it in a new window by clickining 'zoom'
# to see the dendrogram more clearly. You can also google 'hclust cran-r' to find tutorials to make
# a better plot. we can plot it again without labels, to see the structure a bit better:

plot(hclust(dist(topic.words)))

# topics that end up in the same clusters we interpret as being related in some fashion.

# Now, if we want to get really fancy, we can make a network visualization of how 
# topics interlink due to their distribution in documents. The next bit of code does 
# that, and saves in .graphml format, which packages like Gephi http://gephi.org can read.

topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents$article_id

#install.packages("cluster")
library(cluster)
topic_df_dist <- as.matrix(daisy(t(topic_docs), metric = "euclidean", stand = TRUE))
# Change row values to zero if less than row minimum plus row standard deviation
# keep only closely related documents and avoid a dense spagetti diagram
# that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
topic_df_dist[ sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) )) > 0 ] <- 0

#install.packages("igraph") 
# the line above would normally install igraph. However, the latest version is not compatible
# with this version of R. Thus, go to command line in DHBox, cd to the R folder, cd x86_64-pc-linux-gnu-library, cd 3.0 folder.
# wget the older version of igraph that'll work: https://cran.r-project.org/src/contrib/Archive/igraph/igraph_0.6-3.tar.gz
# then, at command line, run the following R command: $ R CMD INSTALL igraph_0.6-3.tar.gz
# this'll install igraph. I
# ndeed, for any package you can look for older versions of it by slotting in
# the name of the package in the url above and browsing the archive.
# Remember, we're working with R version 3.03, from 2013, so we need stuff earlier than that.

# once installed, call it:
library(igraph)

# we transform the information from the previous code block into a network
g <- as.undirected(graph.adjacency(topic_df_dist))

# then we specify the layout and the number of iterations to make it pretty
layout1 <- layout.fruchterman.reingold(g, niter=100)

#then we plot it out
plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1, vertex.color= "grey", edge.arrow.size = 0, vertex.label.dist=0.5, vertex.label = NA)

# when you look at this network, you can see clusters of documents by virtue of largely shared topics
# we export this data in a text format called 'graphml', which can be opened by 
# any text editor, and visualized in nearly any network analysis program
# for further refinement and analysis. It might be interesting to explore why some issues are so
# topically focussed, for instance.

write.graph(g, file="cnd.graphml", format="graphml")

# There are many ways of visualizing and transforming our data. This document only 
# captures a small fraction of the kinds of things you could do. Another good exploration 
# is at http://bridge.library.wisc.edu/hw1a-Rcoding-Jockers.html. Ben Marwick does really
# fun things with the Day of Archaeology blog posts https://github.com/benmarwick/dayofarchaeology 
# and indeed, some of the code above comes from Marwick’s explorations. 
# Keep your R scripts in your open notebook, and somebody might come along and use them, 
# cite them, improve them, share them! Keep also all your data. Here’s an example from my own work https://github.com/shawngraham/ferguson.


