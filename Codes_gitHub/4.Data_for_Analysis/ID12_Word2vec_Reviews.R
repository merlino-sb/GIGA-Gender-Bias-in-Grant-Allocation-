
#####################################################################################
# Script id: 12
# Description: This script runs Word2Vec to generate evaluative terms of reviews
# Author: GIGA team
# Last update: 24.04.2021
#####################################################################################



##-- Libraries

library(doParallel)
library(dplyr)
library(purrr)
library(tidyr)
library(wordVectors)
library(tidytext)
library(factoextra)
library(stringr)
library(readr)


# Upload dataset for the analysis
 load("C:/Users/Stefano/Desktop/GIGA_Project/Scripts/Word2vec/Data_for_analysis.RData")
 rm(dat_1st, dat_3rd)

# Rename variables
 dat <- dat_2nd %>% 
   unite(UT, ProjectNumber_FP, AU, remove = T) %>%
   dplyr::select(UT, Q_Corpus) %>%
   rename(AB = Q_Corpus) %>%
   select(UT, AB)
 
 rm(dat_2nd)

# Get stop words from tidytext
 data("stop_words")

# Add our own stop words
 custom_stop_word <- c("rights","elsevier","b.v","reserved","study", "paper", "result", "model",  "models", "approach", "article", "author", "method", "understand", "focus", "examine", "aim", "argue", "identify",
                       "increase", "datum", "potential", "explore", "include", "issue", "propose", "address", "apply", "require", "analyse", "relate", "finding",
                       "analyze", "discuss", "contribute", "publish", "involve", "draw", "lead", "exist", "set", "reduce", "create", "form", "explain", "play",
                       "affect", "regard", "associate", "establish", "follow", "conclude", "define", "strong", "attempt", "finally")

# Tokenize abstracts
 ABs_df <- data_frame(AB = as.character(dat$AB),
                      UT = as.character(dat$UT))%>%
   filter(AB != "" | AB == !is.na(AB)) %>% 
   filter(UT != "" | UT == !is.na(UT)) 
 
 AB_token <- ABs_df %>% 
   unnest_tokens(word, AB) %>% 
   anti_join(stop_words) %>%
   filter(!word %in% custom_stop_word)
 
 AB_token <- AB_token %>% mutate(word= (tolower(word)))

# Obtain unigrams
 nested_AB <- AB_token %>%
   nest(word) %>%
   mutate(word = map(data, unlist), 
          word = map_chr(word, paste, collapse = " ")) %>% select( -data)
 
 temp <- strsplit(nested_AB$word, split=" ")
 
 nested_AB$nword <- sapply(temp, length)
 
# Remove reviews words appearing less than 15 times
 reviews <- nested_AB %>% filter(nword >(15)) %>% select(word)


# Set the nr. of cores for parallel processing
 cl <- makeCluster(8)
 registerDoParallel(cl)
 
 foreach(i=1: 1:8) %dopar% {
   setwd("C:/Users/Stefano/Desktop/GIGA_Project/Scripts/Word2vec/Reviews")  # IMPORTANT: Create a dedicated folder "Reviews" to store all .txt !!!
 }
 foreach(i=1: 1:dim(reviews)[1]) %dopar% {
   write.table(reviews[i,"word"], file = paste(c("review_",i,".txt"),collapse = ""), sep = ";",
               row.names = TRUE,col.names = ",")}
 
 print("Writting txt files finished")


# Train the Word2vec
 prep_word2vec(origin="Reviews",destination="reviews_bundle.txt", lowercase=T, bundle_ngrams=2, threshold = 50)

 print("prep word2vec finished")

 train_word2vec("reviews_bundle.txt","reviews_model_b2_t50_300_w7_i30_ns15.bin",
                vectors=300,
                threads=8,
                window=7,
                iter=30,
                min_count = 5,
                negative_samples=15,force = T)

 print('model trained')

 model = read.vectors("reviews_model_b2_t50_300_w7_i30_ns15.bin")


# Cosine similarity with selected terms
 print(model %>% closest_to(~"principal"+"investigator", 100))
 print(model %>% closest_to(~"investigator",40))
 print(model %>% closest_to(~"team",40))


 #-- Robustness check via k-mean clustering
 #modgap <- fviz_nbclust(model@.Data[2:2000,], kmeans, k.max = 50, method = "gap_stat")
 #print(modgap) # Guarda il numero ottimale di clusters
 #
 #kmeans_top <- function(data,nclus,ntop){
 #  set.seed(2)
 #  km.res <<- kmeans(data, iter.max = 250,nclus, nstart = 25)
 #  subject_words <- matrix(0,ntop,nclus)
 #  for(i in 1:nclus){
 #    subject_i <- as.data.frame(km.res$cluster[km.res$cluster==i])
 #    subject_words[,i] <- rownames(subject_i)[1:ntop]
 #  }
 #  subject_words <<- as_tibble(subject_words)
 #  subject_words
 #}
 #
 ## IMPORTANT: Change the number of optimal cluster identified above
 #kmeans_top(model@.Data[2:2000,],15,200)   
 #
 #rm(AB_token,abreviation,ABs_df,cl,nested_AB,stop_words,reviews,temp,km.res,i,j,index,custom_stop_word,kmeans_top)
 #save.image(file="word2vec_kmean_reviews.RData")


#--- We identify the following frequent evaluative terms:
#internationally_recognized
#connected_internationally
#expert
#mid_career
#international_connection
#longstanding
#list_publications
#belong_leading
#international_standing
#cv
#track_record
#senior
#capacity_carry
#networked
#recognized
#leader
#internationally_connected 
#qualified_conduct
#considerable_experience
#influential
#credible
#distinguished
#world_renowned
#excellent
#expertise
#experience
#international_standing
#adequate
#leading
#outstanding
#qualified
#track_record
#suitable
#standing
#extensive
#reputation
#leader
#highly_qualified
#qualifications
#internationally_competitive
#leadership
#international_reputation
#excellent_track
#distinguished
#world_leading
