#' Function to tell GOdata from which score genes are differential
#' @param allScore A vector with 1's for signature genes and 0's for non-signature genes
#' @return Boolean vector
#' 
differential <- function (allScore) 
{
  return(allScore==1)
}

#' Find the enriched GO terms for given genes
#' 
#' Takes human genes and finds the enriched GO terms for the signature. Additionally,
#' printes extra information about the enriched GO terms
#' @param all_genes Named vector where names are genes, and value is 1 if it's part of signature, 0 if it's not
#' @param outfolder Outfolder to write the results to
#' @param siganture_name Name of the signature
#' @param pvalue p-value at which to consider an enriched GO term significant
#' 
enrichment <- function(all_genes, outfolder, signature_name, pvalue=0.05){
  ontologies <- c('CC','MF','BP')
  signature_affy_ids <- names(all_genes[all_genes==1])
  signature_GO_mapping <- select(hgu133a.db, signature_affy_ids, 'GO')
  GO_terms <- select(GO.db, signature_GO_mapping$GO,"TERM","GOID")
  signature_GO_terms <- unique(merge(signature_GO_mapping, GO_terms,
                                     by.x = "GO",by.y="GOID"))
  GO_table_list <- list()
  for (ontology in ontologies){
    godata_file <- paste(system.file('data', package='drugFinder'),'/GOData_',signature_name,'_',ontology,'.Rdata',sep="")
      GOdata <- new("topGOdata",
                    description = "Simple session", ontology = ontology,
                    allGenes = all_genes, geneSel = differential,
                    nodeSize = 10,
                    annot = annFUN.db, affyLib = 'hgu133a.db')

    
    resultFisher_file <- paste(system.file('data', package='drugFinder'),'/resultFisher_',signature_name,'_',ontology,'.Rdata',sep="")
      resultFisher <- runTest(GOdata, algorithm = "classic", statistic = "fisher")

    classicFisher_outfile <- paste(outfolder,'GO_results/',ontology,'/',signature_name,'_',ontology,'.xls', sep='')
      allRes_classicFisher <- GenTable(GOdata, classicFisher = resultFisher,
                                       orderBy = "classicFisher", 
                                       topNodes = sum(attributes(resultFisher)$score<pvalue/length(attributes(resultFisher)$score)),
                                       numChar=200)
      names(allRes_classicFisher)[6] <- 'pvalue'
      write.table(allRes_classicFisher, file=classicFisher_outfile, row.names=FALSE,quote=FALSE,sep='\t')
      print(paste('wrote results to ',classicFisher_outfile))
    

    classicFisher_png <- paste(outfolder,'trees/',ontology,'/',signature_name,'_',ontology,'.pdf',sep='')
      cairo_pdf(filename=classicFisher_png, width=8, height=8)
      showSigOfNodes(GOdata, score(resultFisher), firstSigNodes = 10, useInfo ='all',.NO.CHAR=50)
      graphics.off()
      print(paste('wrote figure to',classicFisher_png))

    GO_table <- merge(signature_GO_terms, allRes_classicFisher, by.x=c('GO','TERM'), by.y=c('GO.ID','Term'))
    drop_columns <- list('Annotated', 'Significant', 'Expected')
    GO_table <- GO_table[,!colnames(GO_table) %in% drop_columns]
    GO_table_list[[ontology]] <- GO_table
  }

  GO_table_combined <- do.call('rbind', GO_table_list)
  return(list(GO_table=GO_table_combined,
              enrichment_result=resultFisher,
              godata=GOdata,
              results = allRes_classicFisher))
}

#' Plot overlap in enriched GO terms between the signatures
#' 
#' Takes human genes and finds the enriched GO terms for the signature. Additionally,
#' printes extra information about the enriched GO terms
#' @param enrichment_list A list of signatures and its GO terms
#' @param ylab y-axis description for the enrichment overlap plot (default: '')
#' @param colnames Boolean, if TRUE plot the names of the signatures on the x-axis
#' @param outfolder_location Location of the folder where to write the plots
#' @param GO_results_name Name of the subfolder to write the results in
#' @param main_extra_info Extra info to put in the graph title (default: '')
#'  
enrichment_overlap <- function(enrichment_list,ylab='',colnames=TRUE, outfolder_location,
                               GO_results_name, main_extra_info = ''){
  for (proportion in seq(0,0.85,0.1)){
    terms_BP <- list()
    sets_BP <- list()
    terms_MF <- list()
    sets_MF <- list()
    terms_CC <- list()
    sets_CC <- list()
    terms_all <- list()
    sets_all <- list()
    for (signature in names(enrichment_list)){
      enrichment_result <- enrichment_list[[signature]][[GO_results_name]]
      enrichment_result <- enrichment_result[as.numeric(as.character(enrichment_result$GO.proportion)) >= proportion,]
      terms_BP[[length(terms_BP)+1]] <- enrichment_result[enrichment_result$ONTOLOGY=='BP',]$TERM
      sets_BP[[signature]] <-  enrichment_result[enrichment_result$ONTOLOGY=='BP',]$TERM
      terms_MF[[length(terms_MF)+1]] <- enrichment_result[enrichment_result$ONTOLOGY=='MF',]$TERM
      sets_MF[[signature]] <- enrichment_result[enrichment_result$ONTOLOGY=='MF',]$TERM
      terms_CC[[length(terms_CC)+1]] <- enrichment_result[enrichment_result$ONTOLOGY=='CC',]$TERM
      sets_CC[[signature]] <-  enrichment_result[enrichment_result$ONTOLOGY=='CC',]$TERM
      terms_all[[length(terms_all)+1]] <- enrichment_result$TERM
      sets_all[[signature]] <-  enrichment_result$TERM
    }
    
    terms_BP <- unique(unlist(terms_BP))
    #sets_BP <- sets_BP[names(sets_BP) %in% names(signature_names_shortcut)]
    #names(sets_BP) <- signature_names_shortcut[names(sets_BP)]
    terms_MF <- unique(unlist(terms_MF))
    #sets_MF <- sets_MF[names(sets_MF) %in% names(signature_names_shortcut)]
    #names(sets_MF) <- signature_names_shortcut[names(sets_MF)]
    terms_CC <- unique(unlist(terms_CC))
    #sets_CC <- sets_CC[names(sets_CC) %in% names(signature_names_shortcut)]
    #names(sets_CC) <- signature_names_shortcut[names(sets_CC)]
    terms_all <- unique(unlist(terms_all))
    #sets_all <- sets_all[names(sets_all) %in% names(signature_names_shortcut)]
    #names(sets_all) <- signature_names_shortcut[names(sets_all)]

    dir.create(paste(outfolder_location,'GO.proportion_',proportion,'/',sep=""), showWarnings = FALSE)
    #overlap_plot(sets=sets_MF, terms=terms_MF, outfile=paste(outfolder_location,'GO.proportion_',proportion,'/signature_overlap_MF.pdf',sep=""),
    #             xlab=paste(length(terms_MF)," enriched GO terms",sep=""),ylab=ylab,pch=19, colnames=colnames, width=5+length(terms_MF)/3, mar_lower=14,
    #             main=paste('Molecular function - GO proportion:',proportion, main_extra_info))
    #overlap_plot(sets=sets_BP, terms=terms_BP, outfile=paste(outfolder_location,'GO.proportion_',proportion,'/signature_overlap_BP.pdf',sep=""),
    #             xlab=paste(length(terms_BP)," enriched GO terms",sep=""),ylab=ylab,pch=19, colnames=colnames, width=5+length(terms_BP)/3, mar_lower=14,
    #             main=paste('Biological process - GO proportion:',proportion, main_extra_info))
    #overlap_plot(sets=sets_CC, terms=terms_CC, outfile=paste(outfolder_location,'GO.proportion_',proportion,'/signature_overlap_CC.pdf',sep=""),
    #             xlab=paste(length(terms_CC)," enriched GO terms",sep=""),ylab=ylab,pch=19, colnames=colnames, width=5+length(terms_CC)/3, mar_lower=14,
    #             main=paste('Celular component - GO proportion:',proportion, main_extra_info))
    #overlap_plot(sets=sets_all, terms=terms_all, outfile=paste(outfolder_location,'GO.proportion_',proportion,'/signature_overlap_all.pdf',sep=""),
    #             xlab=paste(length(terms_all)," enriched GO terms",sep=""),ylab=ylab,pch=19, colnames=colnames, width=5+length(terms_all)/3, mar_lower=14,
    #             main=paste('All combined - GO proportion:',proportion, main_extra_info))
    }
}