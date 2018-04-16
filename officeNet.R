##############
#Basic Setup #
##############


library(rio)
library(dplyr)
library(igraph)
library(qgraph)

setwd('C:/Users/nnzarr/Desktop/')

sceneThresh <- 400 #number of scenes the character has to be in to use

if (F){
  raw <- import('the-office-lines.xlsx')
  df_bu <- raw %>% select(season, episode, scene, speaker)
  rm(raw)
  gc()
}

appearances <- df_bu %>%
  mutate(sceneID = sprintf('s%ie%isc%i', season, episode, scene)) %>%
  select(sceneID, speaker)

#determine which characters to include
majorCharacters <- appearances %>%
  group_by(speaker) %>%
  summarize(nScenes = length(unique(sceneID))) %>%
  filter(nScenes >= sceneThresh)
badChars <- c('All') #characters to exclude

#filter appearances by charaters
appearances <- appearances %>% 
  filter(speaker %in% majorCharacters$speaker,
         !speaker %in% badChars)

#find scenes with only one speaker 
singletonScenes <- appearances %>% 
  group_by(sceneID) %>% 
  summarize(nchar=length(unique(speaker))) %>%
  ungroup() %>%
  filter(nchar < 2) %>%
  select(sceneID) 

appearances <- filter(appearances, !sceneID %in% singletonScenes$sceneID)


#Create edgelist
edgelist <- appearances %>%
  group_by(sceneID) %>% 
  do(data.frame(t(combn(sort(unique(.$speaker)), 2)))) %>% #get unique pairs of speakers for that scene
  ungroup() %>%
  select(X1,X2) %>%
  group_by(X1,X2) %>%
  summarize(weight=n()) %>%
  rename(from = X1, to = X2)

#create adjacency matrix
usedChars <- unique(c(edgelist$from, edgelist$to))
adjMat <- matrix(0, length(usedChars), length(usedChars), dimnames=list(usedChars, usedChars))
edgeMat <- as.matrix(edgelist)

adjMat[edgeMat[,1:2]] <- strtoi(edgeMat[,3]) #upper quadrant
adjMat[edgeMat[,2:1]] <- strtoi(edgeMat[,3]) #lower quadrant


weightedAdjMat <- t(apply(adjMat, 1, function(x)(x/sum(x))))



#create graph
q <- qgraph(edgelist, directed = FALSE, layout = 'spring')
wq <- qgraph(weightedAdjMat, directed = TRUE) #, layout = 'spring')




########################
#calculate graph stats #
########################

centralityMeasures <- centrality(q1)
centralityPlot(q1)
centralityTable(q1, standardized = FALSE)

clusteringPlot(q1)
#clusteringTable(q1)

cormatrix <- cor_auto(edgelist)

## from: http://psych-networks.com/r-tutorial-identify-communities-items-networks/
#plot(eigen(cormatrix)$values, type="b")
#abline(h=1,col="red", lty = 3)


###########################################
#Create graph with igraph (igrahp sucks?) #
###########################################
#g2 <- graph_from_data_frame(edgelist, directed = FALSE)
#plot(g, layout = layout_components(g), edge.width = E(g)$weight/max(E(g)$weight))
#plot(g,  edge.width = (E(g)$weight/max(E(g)$weight))*10)