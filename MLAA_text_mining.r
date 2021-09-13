library(tm) 
library(SnowballC)
library(ggplot2)
library(ngram)

######## reading in and summary##################################################
#loading corpus
docs <- VCorpus(DirSource("./docs"))
#docs <- tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))

#doc details
print(docs)
class(docs) #Volatile corpus: 42 documents

#contents 
class(docs[[1]]) # plain text document, text document

#summary of document info
doc_info <- as.data.frame(seq_len(42))
i= 1
for (i in 1:length(docs)) {
  
  doc_info$unique_terms[i]<-wordcount(docs[[i]]$content) #unique words in document
  doc_info$characters[i] <- nchar(docs[[i]]$content) #length of document
  language <- unlist(docs[[1]]$meta)
  doc_info$language[i] <- language[11] #document language
  i= i+1
}
doc_info$`seq_len(42)` <- NULL
write.csv(file="doc_info.csv", doc_info)

############INITIAL PREPROCESS################################################

#Remove punctuation: replaced with " "
docs <- tm_map(docs, removePunctuation)

#all to lower case
docs <- tm_map(docs,content_transformer(tolower))

#remove digits
docs <- tm_map(docs, removeNumbers)

#Remove standard stopwords from list 
docs <- tm_map(docs, removeWords, stopwords("english"))

#remove custom stop words
custom_stopwords <- c("will", "can", "may", "like", "howev", "get", "also", 
                      "said", "much", "let", "even", "might", "say", "anoth", 
                      "show", "shown", "use", "one", "went", "whatev", 
                      "whatsoev", "whenc", "whose", "throughout","well", "look", 
                      "make", "can", "say","one","way","use",
                      "also","howev","tell","will", "much","need","take",
                      "tend","even", "like","particular","rather","said",
                      "get","well","make","ask","come","end", "first","two",
                      "help","often","may", "might","see","someth","thing","point",
                      "post","look","right","now","think","'ve ",
                      "'re ","anoth","put","set","new","good",
                      "want","sure","kind","larg","yes,","day","etc",
                      "quit","sinc","attempt","lack","seen","awar",
                      "littl","ever","moreov","though","found","abl",
                      "enough","far","earli","away","achiev","draw",
                      "last","never","brief","bit","entir","brief",
                      "great","lot","man","say","well", "seem", "within", "here",
                      "doesnt", "stuff", "havent", "otherwi", "your", "typic", "notic",
                      "upon", "back", "definit", "primarili", "what", "mayb", "lets", 
                      "although", "many", "based", "used", "using", "indeed", "heres", "ibis",
                      "ill", "likely", "called", "however", "things", "means", 
                      "makes", "thats", "given", "done", "another", "achieve", "suggests", "claims",
                      "actually", "figure", "example"
                      
                      )
docs <- tm_map(docs, removeWords, custom_stopwords)

#removal of whitespace for cosmetic purposes  
docs <- tm_map(docs, stripWhitespace)

#collapse dtm - total counts/term
dtm <- DocumentTermMatrix(docs)

freq <- colSums(as.matrix(dtm))

#total number of terms
length(freq) #6823

#sorted word frequency in ascending order
ordered <- order(freq, decreasing=TRUE)

#40 most frequently occurred
freq[head(ordered, 40)] 
most40freq <- data.frame(term=names(freq[head(ordered, 40)]),
                         occurrences=freq[head(ordered, 40)] )
hist_40mostfreq <- ggplot(subset(most40freq, 
                   occurrences>1), #plotting histogram
                    aes(reorder(term,occurrences), 
                      occurrences))
hist_40mostfreq <- hist_40mostfreq + geom_bar(stat="identity") #columns
hist_40mostfreq <- hist_40mostfreq + theme(axis.text.x=element_text(angle=45, hjust=1))
hist_40mostfreq

library(wordcloud) #wordcloud visualisation
set.seed(42)
wordcloud(names(freq),freq,min.freq = most40freq[40,2],colors=brewer.pal(6,"Dark2"))

#write out and inspect 
row.names(most40freq) <- seq_len(40)
write.csv(file="freq_40most.csv",most40freq)
most40freq

#40 least frequently occurred
least_freq <- row.names(as.data.frame(freq[tail(ordered,40)]))
write.csv(file= "40least_freq.csv", least_freq)
least_freq

#removing least frequent 40 words
docs <- tm_map(docs, removeWords, least_freq)

#list terms with min frequency of 100
over100words <- findFreqTerms(dtm,lowfreq=100)
write.csv(file="over100freq.csv", over100words)
over100words

#quartgram
quartgram_tokenizer <-  function(x) unlist(lapply(ngrams(words(x),
                                  4), paste, collapse = " "), use.names = FALSE)

#frequency of quartgrams
freq_quart <- colSums(as.matrix(DocumentTermMatrix(docs,
                              control = list(tokenize = quartgram_tokenizer))))

#41433 terms
length(freq_quart)

#sorted quartgram frequency in ascending order
ord_quart <- order(freq_quart,decreasing=TRUE)

#top 8 most frequently occurring quartgram
customquartgram <- c(row.names(as.data.frame(freq_quart[head(ord_quart,8)])))
write.csv(file="customquartgram.csv", customquartgram)
customquartgram

#removing top 8 quartgram 
docs <- tm_map(docs, removeWords, customquartgram)

#trigram
trigram_tokenizer <-  function(x) unlist(lapply(ngrams(words(x),
                          3), paste, collapse = " "), use.names = FALSE)

#frequency of trigrams
freq_tri <- colSums(as.matrix(DocumentTermMatrix(docs,
                            control = list(tokenize = trigram_tokenizer))))
 
#40445 terms
length(freq_tri)

#sorted trigram frequency in ascending order
ord_tri <- order(freq_tri,decreasing=TRUE)

#top 20 most frequently occurring trigram
customtrigram <- row.names(as.data.frame(freq_tri[head(ord_tri, 20)]))
write.csv(file= "customtrigram.csv", customtrigram)
customtrigram

#stemming
docs <- tm_map(docs,stemDocument)

#appending summary info post data preprocess 
i= 1
for (i in 1:length(docs)) {
  
  doc_info$new_unique_terms[i]<-wordcount(docs[[i]]$content) #unique words in document
  doc_info$new_characters[i] <- nchar(docs[[i]]$content) #length of document
  i= i+1
}

write.csv(file="doc_info.csv", doc_info)

############TOPIC MODELS#########################################################
library(topicmodels)
library(dplyr)
#DTM
dtm <- DocumentTermMatrix(docs)
dtm

#setting LDA parameters
burnin <- 1000 # allow to reached equilibrium
iter <- 2000 # iterations
thin <- 500 # thinning 
nstart <- 5 #starting points
seed <- list(23, 5893, 102377, 36489894, 23556) # set seed
best <- TRUE # best result return
k <- 3 # number of topics

#LDA: Latent Dirichlet Allocation
lda <- LDA(dtm,k, method="Gibbs", control=
             list(nstart=nstart, seed = seed, best=best, burnin = burnin,
                  iter = iter, thin=thin))

lda_topics <-as.matrix(topics(lda)) #number of topics for LDA
write.csv(lda_topics,file=paste("LDAGibbs",k,"docs_to_topic.csv"))

lda_terms <- as.matrix(terms(lda,42)) #topics for LDA
write.csv(lda_terms,file=paste("LDAGibbs",k,"topics_to_terms.csv"))
as.data.frame(lda_terms)

topic_prob <- as.data.frame(lda@gamma) #extract topic probability
write.csv(topic_prob,file=paste("LDAGibbs",k,"Topic_Probabilities.csv"))

LDAprobs_3<- read.csv("LDAGibbs 3 Topic_Probabilities.csv")
LDAprobs_3 <- LDAprobs_3 %>%
  mutate(mean = rowMeans(LDAprobs_3[,(2:4)])) #average prob for topics per row
sum(LDAprobs_3$mean) #14 = sum of row averages

# K = 4
k <- 4 # number of topics

lda <- LDA(dtm,k, method="Gibbs", control=
             list(nstart=nstart, seed = seed, best=best,
                  burnin = burnin, iter = iter, thin=thin))

lda_topics <-as.matrix(topics(lda)) #number of topics for LDA
write.csv(lda_topics,file=paste("LDAGibbs",k,"docs_to_topic.csv"))

lda_terms <- as.matrix(terms(lda,42)) #topics for LDA
write.csv(lda_terms,file=paste("LDAGibbs",k,"topics_to_terms.csv"))
as.data.frame(lda_terms)

topic_prob <- as.data.frame(lda@gamma) #extract topic probability
write.csv(topic_prob,file=paste("LDAGibbs",k,"Topic_Probabilities.csv"))
LDAprobs_4<- read.csv("LDAGibbs 4 Topic_Probabilities.csv")
LDAprobs_4 <- LDAprobs_4 %>%
  mutate(mean = rowMeans(LDAprobs_4[,(2:5)]))  #average prob for topics per row
sum(LDAprobs_4$mean) #10.5 = sum of row averages

#k=5
k <- 5 # number of topics

lda <- LDA(dtm,k, method="Gibbs", control=
             list(nstart=nstart, seed = seed, best=best, burnin = burnin,
                  iter = iter, thin=thin))

lda_topics <-as.matrix(topics(lda)) #number of topics for LDA
write.csv(lda_topics,file=paste("LDAGibbs",k,"docs_to_topic.csv"))

lda_terms <- as.matrix(terms(lda,42)) #topics for LDA
write.csv(lda_terms,file=paste("LDAGibbs",k,"topics_to_terms.csv"))
as.data.frame(lda_terms)

topic_prob <- as.data.frame(lda@gamma) #extract topic probability
write.csv(topic_prob,file=paste("LDAGibbs",k,"Topic_Probabilities.csv"))
LDAprobs_5<- read.csv("LDAGibbs 5 Topic_Probabilities.csv")
LDAprobs_5 <- LDAprobs_5 %>%
  mutate(mean = rowMeans(LDAprobs_5[,(2:6)]))  #average prob for topics per row
sum(LDAprobs_5$mean) #8.4 =  sum of row averages

#k=2
k <- 2 # number of topics

lda <- LDA(dtm,k, method="Gibbs", control=
             list(nstart=nstart, seed = seed, best=best, burnin = burnin,
                  iter = iter, thin=thin))

lda_topics <-as.matrix(topics(lda)) #number of topics for LDA
write.csv(lda_topics,file=paste("LDAGibbs",k,"docs_to_topic.csv"))

lda_terms <- as.matrix(terms(lda,42)) #topics for LDA
write.csv(lda_terms,file=paste("LDAGibbs",k,"topics_to_terms.csv"))
as.data.frame(lda_terms)

topic_prob <- as.data.frame(lda@gamma) #extract topic probability
write.csv(topic_prob,file=paste("LDAGibbs",k,"Topic_Probabilities.csv"))
LDAprobs_2<- read.csv("LDAGibbs 2 Topic_Probabilities.csv")
LDAprobs_2 <- LDAprobs_2 %>%
  mutate(mean = rowMeans(LDAprobs_4[,(2:3)]))  #average prob for topics per row
sum(LDAprobs_2$mean) #13.24=  sum of row averages

#k=3 optimum

LDAterms_3<- read.csv("LDAGibbs 3 topics_to_terms.csv")
k3_docs_to_topics <-read.csv("LDAGibbs 3 docs_to_topic.csv")
k3_topic_prob <- round(read.csv("LDAGibbs 3 Topic_Probabilities.csv"),3)
LDA_k_3 <- cbind(LDAterms_3, k3_topic_prob[,(2:4)], k3_docs_to_topics[,2])
colnames(LDA_k_3) <- c("doc", "topic_1", "topic_2", "topic_3", "topicprob_1", 
                       "topicprob_2", "topicprob_3", "doc_to_topic")

############VISUALISATION AND CHARACTERISATION##################################
library(cluster)
library(LICORS)

############kMEANS CLUSTER SET UP################################################
dtm_matrix <- as.matrix(dtm)

#cosine distance measure - chosen because hierarchical not normalised
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

rownames(dtm_matrix) <- 1:length(docs) #renaming matrix rows
CSmatrix <- cosineSim(dtm_matrix) #cosine similarity matrix
csdist <- 1-CSmatrix #cosine distance

#NUMBER OF CLUSTERS VS WSS
wss <- seq(2,41,1)
for (i in seq(2,41,1)) wss[i] <- sum(kmeans(csdist,centers=i)$withinss)
plot(seq(2,41,1), wss[seq(2,41,1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", axes = TRUE) 
for (i in 1:length(docs)) {
  kfit
}
kfit[]
#kmeans for k=6,7,8,9
kfit3[4]#k=3
kfit3 <- kmeans(csdist, 3) #kmeans clustering
clusplot(as.matrix(csdist), # cluster plot
         kfit3$cluster, color=T, shade=T, labels=2, lines=0)
silh3 <- silhouette(kfit3$cluster, csdist) #silhouette plots and widths
silh3
plot(silh3) 

#k=4
kfit4 <- kmeans(csdist, 4) # kmean clustering
clusplot(as.matrix(csdist), # cluster plot
         kfit4$cluster, color=T, shade=T, labels=2, lines=0) 
silh4 <- silhouette(kfit4$cluster, csdist) #silhouette plots and widths
silh4
plot(silh4) 

#k=5
kfit5 <- kmeans(csdist, 5)# kmean clustering
clusplot(as.matrix(csdist), # cluster plot
         kfit5$cluster, color=T, shade=T, labels=2, lines=0)
silh5 <- silhouette(kfit5$cluster, csdist) #silhouette plots and widths
silh5
plot(silh5) 

#k=6
kfit6 <- kmeans(csdist, 6)# kmean clustering
clusplot(as.matrix(csdist), # cluster plot
         kfit6$cluster, color=T, shade=T, labels=2, lines=0)
silh6 <- silhouette(kfit6$cluster, csdist) #silhouette plots and widths
silh6
plot(silh6) 
#k=7
kfit7 <- kmeans(csdist, 7)# kmean clustering
clusplot(as.matrix(csdist), # cluster plot
         kfit7$cluster, color=T, shade=T, labels=2, lines=0)
silh7 <- silhouette(kfit7$cluster, csdist) #silhouette plots and widths
silh7
plot(silh7) 
#k=8
kfit8 <- kmeans(csdist, 8)# kmean clustering
clusplot(as.matrix(csdist), # cluster plot
         kfit8$cluster, color=T, shade=T, labels=2, lines=0)
silh8 <- silhouette(kfit8$cluster, csdist) #silhouette plots and widths
silh8
plot(silh8) #0.223

#k=9
kfit9 <- kmeans(csdist, 9)# kmean clustering
clusplot(as.matrix(csdist), # cluster plot
         kfit9$cluster, color=T, shade=T, labels=2, lines=0)
silh9 <- silhouette(kfit9$cluster, csdist) #silhouette plots and widths
silh9
plot(silh9) #0.17 average silhouette width

#k=10
kfit10 <- kmeans(csdist, 10)# kmean clustering
clusplot(as.matrix(csdist), # cluster plot
         kfit10$cluster, color=T, shade=T, labels=2, lines=0)
silh10 <- silhouette(kfit10$cluster, csdist) #silhouette plots and widths
silh10
plot(silh10) #0.15 average silhouette width

#best k: 5
kfit5$cluster
LDA_k_3$cluster <- kfit5$cluster # appending cluster group to topics
anatomy <- LDA_k_3 %>%
  arrange(cluster)
write.csv(file="anatomy.csv", anatomy)

#cluster groups
#cluster 1
cluster1 <- anatomy %>%
  filter(cluster == 1) %>%
  select(doc)
#cluster 2
cluster2 <- anatomy %>%
  filter(cluster == 2) %>%
  select(doc)
#cluster 3
cluster3 <- anatomy %>%
  filter(cluster == 3) %>%
  select(doc)
#cluster 4
cluster4 <- anatomy %>%
  filter(cluster == 4) %>%
  select(doc)
#cluster 5
cluster5 <- anatomy %>%
  filter(cluster == 5) %>%
  select(doc)

#########NETWORK GRAPHS#########################################################
library(tm)
library(igraph)

#adjacency matrix
CSmatrix[CSmatrix < max(CSmatrix)/2] <- 0 #<half of max cosine similarity excl
CSmatrix <- round(CSmatrix,3)
networkgraph <- graph.adjacency(as.matrix(CSmatrix), weighted=T, mode = "undirected")
set.seed(1)

#Network Graphs and Community detection - Fast/Greedy
fg_comm <- fastgreedy.community(networkgraph) #making communities
V(networkgraph)$color <- fg_comm$membership #adding colour to community
plot(networkgraph, layout=layout.auto(networkgraph)) #network graph plot
file_ref <- cbind(row.names(docs),fg_comm$membership) #key: doc and communities

#adjusting aesthetics of the FG community network graph
V(networkgraph)$label.cex <-degree(networkgraph) / max(degree(networkgraph)) +
  0.7 #vertex label proportionate to connection number
V(networkgraph)$label.color <- "black" #label colour black
E(networkgraph)$color <- "black" #edge colour to black
E(networkgraph)$width <- E(networkgraph)$weight*3 #edge weighted to similarity
plot(networkgraph, layout=layout.auto(networkgraph)) #adjusted plot

#adjusting aesthetics of the cluster network graph
set.seed(1)
V(networkgraph)$color <- LDA_k_3$cluster
V(networkgraph)$label.cex <-degree(networkgraph) / max(degree(networkgraph)) + 
  0.7 #vertex label proportionate to connection number
V(networkgraph)$label.color <- "black" #label colour black
E(networkgraph)$color <- "black" #edge colour to black
E(networkgraph)$width <- E(networkgraph)$weight*3 #edge weighted to similarity
plot(networkgraph, layout=layout.auto(networkgraph)) #adjusted plot

#appending community to anatomy
LDA_k_3$FGcommunity <- fg_comm$membership
anat_community <- LDA_k_3 %>% arrange(FGcommunity)
write.csv(file="anatomy+community.csv", anat_community)

##############LATENT SEMANTIC ANALYSIS#################################
library(tm) 
library(lsa)
#creating tdm object and converting to matrix
tdm <- TermDocumentMatrix(docs) #tdm
tdm_matrix <- as.matrix(tdm) #matrix

#weighting: a (i,j) = L(i,j)*G(i).
tdm_matrixLSA <- lw_tf(tdm_matrix) * gw_idf(tdm_matrix)

#creating Latent semantic space
LSA_space <- lsa(tdm_matrixLSA, dimcalc_share()) 
LSA_matrix <- as.textmatrix(LSA_space) #converted to matrix

#LSA: Analysis 
#cluster 1 
freq_c1 <- colSums(as.matrix(dtm)[cluster1$doc,])

#sorted cluster 1 term frequency in ascending order
ord_c1 <- order(freq_c1,decreasing=TRUE)

#top 3 most frequently occurring in cluster 1
mostfreq_termc1 <- c(row.names(as.data.frame(freq_c1[head(ord_c1,3)])))
mostfreq_termc1 

#appending LSA term vector to cluster1 
cluster1$map <- LSA_matrix[mostfreq_termc1[1],cluster1$doc] 
cluster1$issu <- LSA_matrix[mostfreq_termc1[2],cluster1$doc] 
cluster1$question <- LSA_matrix[mostfreq_termc1[3],cluster1$doc] 

#cluster 2 
freq_c2 <- colSums(as.matrix(dtm)[cluster2$doc,])

#sorted cluster 2 term frequency in ascending order
ord_c2 <- order(freq_c2,decreasing=TRUE)

#top 3 most frequently occurring in cluster 2
mostfreq_termc2 <- c(row.names(as.data.frame(freq_c2[head(ord_c2,3)])))
mostfreq_termc2

#appending LSA term vector to cluster2 
cluster2$document <-LSA_matrix[mostfreq_termc2[1],cluster2$doc] 
cluster2$cluster <-LSA_matrix[mostfreq_termc2[2],cluster2$doc]
cluster2$word <-LSA_matrix[mostfreq_termc2[3],cluster2$doc] 

#cluster 3 
freq_c3 <- colSums(as.matrix(dtm)[cluster3$doc,])

#sorted cluster 3 term frequency in ascending order
ord_c3 <- order(freq_c3,decreasing=TRUE)

#top 3 most frequently occurring in cluster 3
mostfreq_termc3 <- c(row.names(as.data.frame(freq_c3[head(ord_c3,3)])))
mostfreq_termc3 

#appending LSA term vector to cluster3 
cluster3 <- cluster3 %>%
  mutate(risk = LSA_matrix[mostfreq_termc3[1],cluster3$doc]) %>% 
  mutate(manag = LSA_matrix[mostfreq_termc3[2],cluster3$doc]) %>%
  mutate(project = LSA_matrix[mostfreq_termc3[3],cluster3$doc])

#cluster 4 
freq_c4 <- colSums(as.matrix(dtm)[cluster4$doc,])

#sorted cluster 4 term frequency in ascending order
ord_c4 <- order(freq_c4,decreasing=TRUE)

#top 3 most frequently occurring in cluster 4
mostfreq_termc4 <- c(row.names(as.data.frame(freq_c4[head(ord_c4,3)])))
mostfreq_termc4 

#appending LSA term vector to cluster4 
cluster4 <- cluster4 %>%
  mutate(task = LSA_matrix[mostfreq_termc4[1],cluster4$doc]) %>% 
  mutate(time = LSA_matrix[mostfreq_termc4[2],cluster4$doc]) %>% 
  mutate(distribut = LSA_matrix[mostfreq_termc4[3],cluster4$doc])

#cluster 5 
freq_c5 <- colSums(as.matrix(dtm)[cluster5$doc,])

#sorted cluster 5 term frequency in ascending order
ord_c5 <- order(freq_c5,decreasing=TRUE)

#top 3 most frequently occurring in cluster 5
mostfreq_termc5 <- c(row.names(as.data.frame(freq_c5[head(ord_c5,3)])))
mostfreq_termc5

#appending LSA term vector to cluster5 
cluster5 <- cluster5 %>%
  mutate(project = LSA_matrix[mostfreq_termc5[1],cluster5$doc]) %>% 
  mutate(manag = LSA_matrix[mostfreq_termc5[2],cluster5$doc]) %>% 
  mutate(organis = LSA_matrix[mostfreq_termc5[3],cluster5$doc]) 

#top 3 most frequent words for each cluster
mostfreq_percluster <- data.frame(mostfreq_termc1,mostfreq_termc2,mostfreq_termc3,
                                  mostfreq_termc4,mostfreq_termc5)
write.csv(file="mostfreqterm_percluster.csv", mostfreq_percluster)

#LSA for each cluster
write.csv(file="LSAcluster1.csv", cluster1)
write.csv(file="LSAcluster2.csv", cluster2)
write.csv(file="LSAcluster3.csv", cluster3)
write.csv(file="LSAcluster4.csv", cluster4)
write.csv(file="LSAcluster5.csv", cluster5)

#Similarity matrix from LSA_matrix
CS_LSA <- as.matrix(cosineSim(t(LSA_matrix)))
CS_LSA
cor(CS_LSA)
library(corrplot)
cor_matrix_cs_lsa <-corrplot(CS_LSA)
cor_matrix_cs_lsa
CS_LSA[CS_LSA < 0.75] <- 0
CS_LSAtrunc <- round(CS_LSA,3)
corrplot(CS_LSAtrunc)

########################WORD EMBEDDING###################################
library(text2vec)

#iterator
token <- space_tokenizer(docs)

#vocab creation
it <- itoken(token, progressbar = FALSE)
vocab <- create_vocabulary(it)

#vocab pruned by word frequency <4 
vocab <- prune_vocabulary(create_vocabulary(it), term_count_min = 5)

#filter vocab
vectorizer <- vocab_vectorizer(vocab)# Create Term Co-occurence matrix

#context window 5 words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

#glove: train
glove <-  GlobalVectors$new(rank = 50, x_max = 5)
wordvec_main <- glove$fit_transform(tcm, n_iter = 5)

#word context vectors
wordvec_context <- glove$components
wordvec <- wordvec_main + t(wordvec_context)

#cluster 1 vector "map'
word1 <- wordvec_main["map", , drop = FALSE]
cos_sim <- sim2(x = wordvec_main, y = word1, method = "cosine", norm = "l2")
highfreq_closestword <- as.data.frame(c(rownames(as.data.frame(head(sort(cos_sim[,1],
                                                                         decreasing = TRUE), 5))))) #appending closest words
highfreq_closestword$V1 <- highfreq_closestword$`c(rownames(as.data.frame(head(sort(cos_sim[, 1], decreasing = TRUE), 5))))`
highfreq_closestword$`c(rownames(as.data.frame(head(sort(cos_sim[, 1], decreasing = TRUE), 5))))` <- NULL

closeness2highestfreq <- as.data.frame(c(head(sort(cos_sim[,1],
                                                   decreasing = TRUE), 5))) #appending closeness
closeness2highestfreq <- closeness2highestfreq %>% 
  rename(cluster1 = `c(head(sort(cos_sim[, 1], decreasing = TRUE), 5))`)

#cluster 2 vector "document"
word2 <- wordvec_main["document", , drop = FALSE] 
cos_sim1 <- sim2(x = wordvec_main, y = word2, method = "cosine", norm = "l2")
closeness2highestfreq$cluster2 <- head(sort(cos_sim1[,1], decreasing = TRUE), 5) #closeness of words
highfreq_closestword$V2 <- c(rownames(as.data.frame(head(sort(cos_sim1[,1], decreasing = TRUE), 5))))#closest words

#cluster 3 vector "risk"
word3 <- wordvec_main["risk", , drop = FALSE] 
cos_sim3 <- sim2(x = wordvec_main, y = word3, method = "cosine", norm = "l2")
closeness2highestfreq$cluster3 <-head(sort(cos_sim3[,1], decreasing = TRUE), 5)#closeness of words
highfreq_closestword$V3 <- c(rownames(as.data.frame(head(sort(cos_sim3[,1], decreasing = TRUE), 5))))#closest words

#cluster 4 vector -"task"
word4 <- wordvec_main["task", , drop = FALSE] +wordvec_main["question", , drop = FALSE] 
cos_sim4 <- sim2(x = wordvec_main, y = word4, method = "cosine", norm = "l2")
closeness2highestfreq$cluster4 <-head(sort(cos_sim4[,1], decreasing = TRUE), 5)#closeness of words
highfreq_closestword$V4 <- c(rownames(as.data.frame(head(sort(cos_sim4[,1], decreasing = TRUE), 5))))#closest words

#cluster 5 vector -"project"
word5 <- wordvec_main["project", , drop = FALSE] +
  wordvec_main["issu", , drop = FALSE] 
cos_sim5 <- sim2(x = wordvec_main, y = word5, method = "cosine", norm = "l2")
closeness2highestfreq$cluster5 <-head(sort(cos_sim5[,1], decreasing = TRUE), 5)#closeness of words
highfreq_closestword$V5 <- c(rownames(as.data.frame(head(sort(cos_sim5[,1], decreasing = TRUE), 5))))#closest words

#closest 5 word to highest frequency term of each cluster
colnames(highfreq_closestword) <- c("c1_map", "c2_document", "c3_risk", "c4_task", "c5_project")

#closeness of closest 5 words to highest frequency term of each cluster
colnames(closeness2highestfreq) <-  c("c1_map", "c2_document", "c3_risk", "c4_task", "c5_project")

#appended results written out
write.csv(file="closeness2highestfreq.csv", closeness2highestfreq)
write.csv(file="highfreq_closestword.csv", highfreq_closestword)

####################characteristing of communities############################
#community frequencies 
freq_fg_1 <- colSums(as.matrix(dtm)[which(LDA_k_3$FGcommunity ==1),])
fg_3most_freq <- list(freq_fg_1[head(order(freq_fg_1,decreasing = TRUE),3)])
freq_fg_2 <- colSums(as.matrix(dtm)[which(LDA_k_3$FGcommunity ==2),])
freq_fg_3 <- colSums(as.matrix(dtm)[which(LDA_k_3$FGcommunity == 3),])
freq_fg_4 <- colSums(as.matrix(dtm)[which(LDA_k_3$FGcommunity ==4),])
freq_fg_5 <- colSums(as.matrix(dtm)[which(LDA_k_3$FGcommunity ==5),])
freq_fg_6 <- colSums(as.matrix(dtm)[which(LDA_k_3$FGcommunity ==6),])
freq_fg_7 <- as.matrix(dtm)[which(LDA_k_3$FGcommunity ==7),]
freq_fg_8 <-  as.matrix(dtm)[which(LDA_k_3$FGcommunity ==8),]
freq_fg_9 <-  as.matrix(dtm)[which(LDA_k_3$FGcommunity ==9),]
freq_fg_10 <-  as.matrix(dtm)[which(LDA_k_3$FGcommunity ==10),]
freq_fg_11 <-  as.matrix(dtm)[which(LDA_k_3$FGcommunity ==11),]
freq_fg_12 <-  as.matrix(dtm)[which(LDA_k_3$FGcommunity ==12),]
freq_fg_13 <-  as.matrix(dtm)[which(LDA_k_3$FGcommunity ==13),]
freq_fg_14 <-  as.matrix(dtm)[which(LDA_k_3$FGcommunity ==14),]

#frequency of most frequent 3 words per community appended to fg_3most_freq
fg_3most_freq <- as.data.frame(fg_3most_freq)
fg_3most_freq[2] <- c(freq_fg_2[head(order(freq_fg_2,decreasing = TRUE),3)])
fg_3most_freq[3] <- c(freq_fg_3[head(order(freq_fg_3,decreasing = TRUE),3)])
fg_3most_freq[4] <- c(freq_fg_4[head(order(freq_fg_4,decreasing = TRUE),3)])
fg_3most_freq[5] <- c(freq_fg_5[head(order(freq_fg_5,decreasing = TRUE),3)])
fg_3most_freq[6] <- c(freq_fg_6[head(order(freq_fg_6,decreasing = TRUE),3)])
fg_3most_freq[7] <- c(freq_fg_7[head(order(freq_fg_7,decreasing = TRUE),3)])
fg_3most_freq[8] <- c(freq_fg_8[head(order(freq_fg_8,decreasing = TRUE),3)])
fg_3most_freq[9] <- c(freq_fg_9[head(order(freq_fg_9,decreasing = TRUE),3)])
fg_3most_freq[10] <- c(freq_fg_10[head(order(freq_fg_10,decreasing = TRUE),3)])
fg_3most_freq[11] <- c(freq_fg_11[head(order(freq_fg_11,decreasing = TRUE),3)])
fg_3most_freq[12] <- c(freq_fg_12[head(order(freq_fg_12,decreasing = TRUE),3)])
fg_3most_freq[13] <- c(freq_fg_13[head(order(freq_fg_13,decreasing = TRUE),3)])
fg_3most_freq[14] <- c(freq_fg_14[head(order(freq_fg_14,decreasing = TRUE),3)])

#appending most frequent 3 words per community to fg_3most_term
fg_3most_term <- as.data.frame(seq_len(3))
fg_3most_term[1] <- c(row.names(as.data.frame(freq_fg_1[head(order(freq_fg_1,decreasing = TRUE),3)])))
fg_3most_term[2] <- c(row.names(as.data.frame(freq_fg_2[head(order(freq_fg_2,decreasing = TRUE),3)])))
fg_3most_term[3] <- c(row.names(as.data.frame(freq_fg_3[head(order(freq_fg_3,decreasing = TRUE),3)])))
fg_3most_term[4] <- c(row.names(as.data.frame(freq_fg_4[head(order(freq_fg_4,decreasing = TRUE),3)])))
fg_3most_term[5] <- c(row.names(as.data.frame(freq_fg_5[head(order(freq_fg_5,decreasing = TRUE),3)])))
fg_3most_term[6] <- c(row.names(as.data.frame(freq_fg_6[head(order(freq_fg_6,decreasing = TRUE),3)])))
fg_3most_term[7] <- c(row.names(as.data.frame(freq_fg_7[head(order(freq_fg_7,decreasing = TRUE),3)])))
fg_3most_term[8] <- c(row.names(as.data.frame(freq_fg_8[head(order(freq_fg_8,decreasing = TRUE),3)])))
fg_3most_term[9] <- c(row.names(as.data.frame(freq_fg_9[head(order(freq_fg_9,decreasing = TRUE),3)])))
fg_3most_term[10] <- c(row.names(as.data.frame(freq_fg_10[head(order(freq_fg_10,decreasing = TRUE),3)])))
fg_3most_term[11] <- c(row.names(as.data.frame(freq_fg_11[head(order(freq_fg_11,decreasing = TRUE),3)])))
fg_3most_term[12] <- c(row.names(as.data.frame(freq_fg_12[head(order(freq_fg_12,decreasing = TRUE),3)])))
fg_3most_term[13] <- c(row.names(as.data.frame(freq_fg_13[head(order(freq_fg_13,decreasing = TRUE),3)])))
fg_3most_term[14] <- c(row.names(as.data.frame(freq_fg_14[head(order(freq_fg_14,decreasing = TRUE),3)])))

#renaming columns and rows
colnames(fg_3most_term) <- seq_len(14)
colnames(fg_3most_freq) <- seq_len(14)
row.names(fg_3most_term) <- seq_len(3)
row.names(fg_3most_freq) <- seq_len(3)

#write out ouput
write.csv(file="fg_3most_term.csv", fg_3most_term)
write.csv(file="fg_3most_freq.csv", fg_3most_freq)

#nearest neighbours for most frequent term per cluster
library(LSAfun)
dim(LSA_space$tk)
LSAtk <- t(LSA_space$sk*t(LSA_space$tk))

#appending proximity of neighbours
NNnearness <- as.data.frame(c(unlist(neighbors(fg_3most_term[1,1],n=6,tvectors=LSAtk))))
NNnearness$b <- c(unlist(neighbors(fg_3most_term[1,2],n=6,tvectors=LSAtk)))
NNnearness[3] <- neighbors(fg_3most_term[1,3],n=6,tvectors=LSAtk)
NNnearness[4] <- neighbors(fg_3most_term[1,4],n=6,tvectors=LSAtk)
NNnearness[5] <- neighbors(fg_3most_term[1,5],n=6,tvectors=LSAtk)
NNnearness[6] <- neighbors(fg_3most_term[1,6],n=6,tvectors=LSAtk)
NNnearness[7] <- neighbors(fg_3most_term[1,7],n=6,tvectors=LSAtk)
NNnearness[8] <- neighbors(fg_3most_term[1,8],n=6,tvectors=LSAtk)
NNnearness[9] <- neighbors(fg_3most_term[1,9],n=6,tvectors=LSAtk)
NNnearness[10] <- neighbors(fg_3most_term[1,10],n=6,tvectors=LSAtk)
NNnearness[11] <- neighbors(fg_3most_term[1,11],n=6,tvectors=LSAtk)
NNnearness[12] <- neighbors(fg_3most_term[1,12],n=6,tvectors=LSAtk)
NNnearness[13] <- neighbors(fg_3most_term[1,13],n=6,tvectors=LSAtk)
NNnearness[14] <- neighbors(fg_3most_term[1,14],n=6,tvectors=LSAtk)

#appending neighbours
NNterm <- as.data.frame(c(unlist(row.names(as.data.frame(neighbors(fg_3most_term[1,1],n=6,tvectors=LSAtk))))))
NNterm$b <- c(unlist(row.names(as.data.frame(neighbors(fg_3most_term[1,2],n=6,tvectors=LSAtk)))))
NNterm[3] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,3],n=6,tvectors=LSAtk))))
NNterm[4] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,4],n=6,tvectors=LSAtk))))
NNterm[5] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,5],n=6,tvectors=LSAtk))))
NNterm[6] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,6],n=6,tvectors=LSAtk))))
NNterm[7] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,7],n=6,tvectors=LSAtk))))
NNterm[8] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,8],n=6,tvectors=LSAtk))))
NNterm[9] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,9],n=6,tvectors=LSAtk))))
NNterm[10] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,10],n=6,tvectors=LSAtk))))
NNterm[11] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,11],n=6,tvectors=LSAtk))))
NNterm[12] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,12],n=6,tvectors=LSAtk))))
NNterm[13] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,13],n=6,tvectors=LSAtk))))
NNterm[14] <- c(row.names(as.data.frame(neighbors(fg_3most_term[1,14],n=6,tvectors=LSAtk))))


#renaming columns and rows and output
colnames(NNnearness) <- seq_len(14)
row.names(NNnearness) <- seq_len(6)
write.csv(file="NNnearness.csv", round(NNnearness,3))

colnames(NNterm) <- seq_len(14)
row.names(NNterm) <- seq_len(6)
write.csv(file="NNterm.csv", NNterm)
