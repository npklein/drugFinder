######################################### Specificity of the GO terms ##################################
load_GO_data <- function(){
  # Load ancestors and offsprings of the GO terms
  if (!exists('ancestors')){
    ancestors <- list()
    ancestors$BP <- as.list(as.character(GOBPANCESTOR)) #biological process
    ancestors$MF <- as.list(as.character(GOMFANCESTOR)) #molecular function
    ancestors$CC <- as.list(as.character(GOCCANCESTOR)) #cellular component
  }
  if (!exists('offsprings')){
    offsprings <- list()
    offsprings$BP <- as.list(as.character(GOBPOFFSPRING))
    offsprings$MF <- as.list(as.character(GOMFOFFSPRING))
    offsprings$CC <- as.list(as.character(GOCCOFFSPRING))
  }
  return(list(ancestors=ancestors, offsprings=offsprings))
}

calculateIC <- function(gene2go, go_term){
  GO_data <- load_GO_data()
  offsprings <- GO_data$offsprings
	go_ic_data <- NULL
	IC <- NULL
  offspring_ <- union(offsprings[go_term], go_term)
	prob <- length(gene2go$GeneID[gene2go$GO_ID %in% offspring_])
	if(prob == 0){
		IC <- NA
	} else {
	  denom <- length(unique(gene2go[,2]))
		IC <- (-log2(prob/denom))
	}
	IC <- round(IC, digits=2)
	return(IC)
}


GO_specificity <- function(GO_dataframe){
  if (!exists('gene2go')){
    load(system.file("data", "gene2go.RData", package="drugFinder"))
  }
  print('load_GO_data')
  GO_data <- load_GO_data()
  
  ancestors <- GO_data$ancestors
  offsprings <- GO_data$offsprings
  # GO_dataframe has to contain a column "GO" with GO ids, and a column 
  # "ONTOLOGY" with the ontology of the GO ids
  results <- matrix(0, ncol=3, nrow=nrow(GO_dataframe))
  colnames(results) <- c('GO ID', 'GO proportion', 'ONTOLOGY')
  i <- 1
  print('calculate GO specifity')
  for(index in 1:nrow(GO_dataframe)){
    if(index %% 200 == 0){
      print(paste(index,'/',nrow(GO_dataframe),' GO term specificity calculated',sep=''))
    }
    GO_ID <- as.character(GO_dataframe[index,]$GO)
    ancestor <- length(ancestors[[GO_dataframe[index,]$ONTOLOGY]][GO_ID][[1]])-1
  	offspring <- length(offsprings[[GO_dataframe[index,]$ONTOLOGY]][GO_ID][[1]])
  	GO_proportion <- 1-(offspring/(offspring+ancestor))
  	#IC <- calculateIC(gene2go, GO_ID)
  	results[i,] <- c(GO_ID, GO_proportion, GO_dataframe[index,]$ONTOLOGY)
  	i <- i+1
  }
  print('Done calculating specificity, returing results')
  return(data.frame(results))
}
#write.table(results, file = 'Function specificity.txt', sep='\t', col.names=T, row.names=F)