edgeToAdj <- function(edgeList) {
  
  #create adjacency matrix from edgeList; edgelist should have 3 columns: from-node, to-node, strength
  
  nodeNames <- unique(c(edgeList[[1]], edgeList[[2]])) 
  adjMat <- matrix(0, length(nodeNames), length(nodeNames), dimnames=list(nodeNames, nodeNames))
  
  edgeMat <- as.matrix(edgeList)
  
  adjMat[edgeMat[,1:2]] <- strtoi(edgeMat[,3]) #upper quadrant
  adjMat[edgeMat[,2:1]] <- strtoi(edgeMat[,3]) #lower quadrant
  
  return(adjMat)
}

filterAppearances <- function(appearanceList, sceneThresh=400, badChars=c('All'), useChars=c(), minChars=2,
                              maxChars=Inf){
  #expecting df with the sceneIds in first column and character name in second column
  #sceneThresh determines how many scenes they have to be in to count
  #determine which characters to include
  #badChars are characters to exclude (default to 'All' as in when they speak collectively)
  
  #filter appearanceList by charaters
  if (length(useChars) == 0){
    majorCharacters <- appearanceList %>%
      group_by(speaker) %>%
      summarize(nScenes = length(unique(sceneID))) %>%
      filter(nScenes >= sceneThresh)
    
    clean_appearanceList <- appearanceList %>% 
      filter(speaker %in% majorCharacters$speaker,
             !speaker %in% badChars)
  }
    else{

      clean_appearanceList <- appearanceList %>%
        filter(speaker %in% useChars)
  }
  
  #find scenes with the wrong number of speakers 
  badScenes <- clean_appearanceList %>% 
    group_by(sceneID) %>% 
    summarize(nchar=length(unique(speaker))) %>%
    ungroup() %>%
    filter((nchar < minChars) | (nchar > maxChars)) %>% #, nchar <= maxChars
    select(sceneID) 
  
  clean_appearanceList <- filter(clean_appearanceList, !sceneID %in% badScenes$sceneID)
  
  return(clean_appearanceList)
}