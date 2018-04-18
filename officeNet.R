##############
#Basic Setup #
##############


library(rio)
library(dplyr)
library(igraph)
library(qgraph)

setwd('C:/Users/nnzarr/Desktop/')
source('C:/Users/nnzarr/Desktop/Dropbox/dataSciencePractice/officeLines/officeHelpers.R')

sceneThresh <- 400 #number of scenes the character has to be in to use

if (T){
  #raw <- import('the-office-lines.xlsx')
  raw <- import('officeLinesSentiment.csv')
  df_bu <- raw %>% select(season, episode, scene, speaker, avgSent)
  rm(raw)
  gc()
}


########################################
# Create and clean list of appearances #
########################################
appearances <- df_bu %>%
  mutate(sceneID = sprintf('s%ie%isc%i', season, episode, scene)) %>%
  select(sceneID, speaker, avgSent) #%>%
  #filter(avgSent > -.2)

#appearances <- appearances  



#removes scenes with one character and characters that don't show up in more scenes than sceneThresh
#can also remove or include specific characters if desired; must be done last

#appearances <- filterAppearances(appearances, sceneThresh=50, useChars = c()) 
clean_appearances <- filterAppearances(appearances, sceneThresh=100, useChars = c(), maxChars = 2)


#Create edgelist
edgelist <- clean_appearances %>%
  group_by(sceneID) %>% 
  do(data.frame(t(combn(sort(unique(.$speaker)), 2)))) %>% #get unique pairs of speakers for that scene
  ungroup() %>%
  select(X1,X2) %>%
  group_by(X1,X2) %>%
  summarize(weight=n()) %>%
  rename(from = X1, to = X2)

##create adjacency matrix
#usedChars <- unique(c(edgelist$from, edgelist$to))
#adjMat <- matrix(0, length(usedChars), length(usedChars), dimnames=list(usedChars, usedChars))
#edgeMat <- as.matrix(edgelist)

#adjMat[edgeMat[,1:2]] <- strtoi(edgeMat[,3]) #upper quadrant
#adjMat[edgeMat[,2:1]] <- strtoi(edgeMat[,3]) #lower quadrant


adjMat <- edgeToAdj(edgelist)

#normalize by total number of interactions per person (so everyone has the same total degree)
weightedAdjMat <- t(apply(adjMat, 1, function(x)(x/sum(x))))



#create graph
#transVals = centralityMeasures$Betweenness * (225/max(centralityMeasures$Betweenness)) + 20
#q <- qgraph(edgelist, directed = FALSE, layout = 'spring', color = 'grey', vTrans=transVals)
q <- qgraph(edgelist, directed = FALSE, layout = 'spring', edge.color = 'green')


wq <- qgraph(weightedAdjMat, directed = TRUE) #, layout = 'spring')


####################
# Create subgraphs #
####################
if(F){
  #graph only looking at connections with one person (automatically weighted)
  
  character <- 'Kevin'
  
  subEdgeList <- edgelist %>% 
    filter(to==character | from==character) 
  
  sq <- qgraph(subEdgeList, directed = FALSE, layout = 'spring')
}

########################
#calculate graph stats #
########################

if (F){
  centralityMeasures <- centrality(q1)
  centralityPlot(q1)
  centralityTable(q1, standardized = FALSE)
  
  clusteringPlot(q1)
  #clusteringTable(q1)
  
  cormatrix <- cor_auto(edgelist)
}

## from: http://psych-networks.com/r-tutorial-identify-communities-items-networks/
#plot(eigen(cormatrix)$values, type="b")
#abline(h=1,col="red", lty = 3)


###########################################
#Create graph with igraph (igrahp sucks?) #
###########################################
#g2 <- graph_from_data_frame(edgelist, directed = FALSE)
#plot(g, layout = layout_components(g), edge.width = E(g)$weight/max(E(g)$weight))
#plot(g,  edge.width = (E(g)$weight/max(E(g)$weight))*10)