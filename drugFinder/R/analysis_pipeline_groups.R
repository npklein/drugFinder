#' Find candidate drugs
#' 
#' Takes in a zebrafish microarray dataset and which samples are control and desired,
#' and uses this to find design signatures and find top drug candidates for inducing 
#' these signatures.
#' @param microarray_data The microarray input data. A dataframe with an ID column, Gene.Symbol column, gene_assignment collumn, and N amount of columns for samples, with normalized expression values. (See microarray_data in drugFinder library)
#' @param datasets Datasets of 
#' @param outfolder_location_root Path to folder where all results are written to (if it doesn't exist, will be created)
#' @param memory_limit Memory limit to set to R (only works in Windows)
#' @docType data
#' @keywords datasets
#' @name zebrafish_conversion_table
#' @usage load(zebrafish_conversion_table)
#' @export
#' 
find_drugs <- function(microarray_data, datasets, outfolder_location_root, memory_limit=800000){
  #find_python_cmd(minimum_version='3.0',required_modules=c('requests','html2text','argparse','urllib','bs4'))
  if (substr(outfolder_location_root, nchar(outfolder_location_root), nchar(outfolder_location_root))!='/'){outfolder_location_root <-paste(outfolder_location_root,'/',sep='')}
  memory.limit(as.numeric(memory_limit))
  dir.create(outfolder_location_root, showWarnings = FALSE)
  rownames(microarray_data) <- make.names(microarray_data[,'Gene.Symbol'], unique=TRUE)
  #check_mapping(conversion_table, outfolder_location_root)
  conversion_table <- conversion_table[,c('Gene.Symbol','entrez')]
  signatures <- list()
  signature_size <- list()
  for (input_dataset_name in names(datasets)){
    #if (length(datasets[[input_dataset_name]]) < 2){stop(paste('Signature group',input_dataset_name,'has less than 2 signatures. Each signature group needs at least 2 signatures to work'))}
    print(paste('Working on group',input_dataset_name))
    outfolder_location <- paste(outfolder_location_root,gsub('\\s','', input_dataset_name),'/',sep='')
    create_folders(outfolder_location)
    for (input_data in datasets[[input_dataset_name]]){
      name1 = input_data$name1
      name2_original = input_data$name2
      selection_list <- gene_selections(microarray_data, input_data)
      for(selection_method in names(selection_list$genes)){
        geneSel <- selection_list[['genes']][[selection_method]]
        if(is.null(dim(geneSel)[[1]]) || dim(geneSel)[[1]]<2){
          print(paste("0 or 1 genes above threshold, not enough for comparisons"))
          next()
        }
        all_locations <- output_location(selection_method, name2_original, outfolder_location)
        print(paste("Comparing ",name1," and ",all_locations$name2))
        geneSel_id <- unique(conversion_table$entrez[match(toupper(rownames(geneSel)),toupper(conversion_table$Gene.Symbol))])
        geneSel_symbol <- toupper(conversion_table$Gene.Symbol[match(toupper(rownames(geneSel)),toupper(conversion_table$Gene.Symbol))])
        geneSel_symbol <- geneSel_symbol[!is.na(geneSel_symbol)]
        gene_list <- selection_list$gene_list[geneSel_symbol,]
        gene_list <- gene_list[order(-gene_list$logFC),]
        gene_list_up <- gene_list[gene_list$logFC > 0,]
        gene_list_down <- gene_list[gene_list$logFC < 0,]
        geneSel_id <- as.character(geneSel_id[!is.na(geneSel_id)])
        print(paste('length signature:',length(geneSel_id)))
        #if(length(geneSel_id)<40){
        signatures[[paste(name1,'__',all_locations$name2,sep='')]] <- list(gene_list=gene_list,gene_ids=geneSel_id, 
                                                            name=paste(name1,"__",all_locations$name2,sep=""),
                                                            gene_list_down=gene_list_down,gene_list_up=gene_list_up,
                                                            gene_list_complete=selection_list$gene_list)
        #}    
        write_summary(outfolder_location, geneSel, geneSel_id, name1, all_locations$name2)
        #plot_results(name1=name1, name2=all_locations$name2, geneSel=geneSel, 
        #             heatmap_location=all_locations$heatmap_location, 
        #             dendogram_location_samples=all_locations$dendogram_location_samples, 
        #             dendogram_location_genes=all_locations$dendogram_location_genes, 
        #             volcano_location=all_locations$volcano_location, 
        #             f=selection_list$f, fit=selection_list$fit)
      }
    }

    # convert all zebra symbols from the microarray to hgu133a affy id (where possible)
    zebra_to_human <- conversion_table[match(toupper(microarray_data[,'Gene.Symbol']), toupper(conversion_table$Gene.Symbol)),]
    zebra_to_human <- zebra_to_human[!is.na(zebra_to_human$entrez),]
    human_to_affy <- entrez_id__to__affy_id[entrez_id__to__affy_id$entrez_id %in% zebra_to_human$entrez,]
    human_to_affy <- human_to_affy[!is.na(human_to_affy$entrez_id),]
    human_probes <- numeric(length=dim(unique(human_to_affy))[1])
    names(human_probes) <- unique(human_to_affy$affy_id)
    
    outfolder_detailed <- paste(outfolder_location,'connectivity_map/detailed_results/',sep="")
    outfolder_permuted <- paste(outfolder_location,'connectivity_map/permuted_results/',sep="")
    cmap_detailed_results <- list()
    cmap_permuted_results_by_name <- list()
    cmap_permuted_results_by_celltype <- list()
    cmap_permuted_results_by_ATC <- list()
    enrichment_list <- list()
    terms_entrez <- list()
    sets_entrez <- list()
    terms_affy <- list()
    sets_affy <- list()
    for (signature in signatures){
      terms_entrez[[length(terms_entrez)+1]] <- signature$gene_ids
      sets_entrez[[signature$name]] <- unique(signature$gene_ids)
      signature_to_affy <- entrez_id__to__affy_id[entrez_id__to__affy_id$entrez_id %in% signature$gene_ids,]
      terms_affy[[length(terms_affy)+1]] <- signature_to_affy$affy_id
      sets_affy[[signature$name]] <- unique(signature_to_affy$affy_id)
      human_probes[intersect(names(human_probes), signature_to_affy$affy_id)] <- 1
      signature_affy_ids <- names(human_probes[human_probes==1])
      #################### functional enrichment stuff ######################
      enrichment_result <- enrichment(human_probes, paste(outfolder_location,'enrichment/',sep=""), signature$name)
      enrichment_specificity <- GO_specificity(enrichment_result$GO_table)  
      enrichment_result$GO_full_results <- merge(enrichment_result$GO_table, enrichment_specificity,
                                 by.x=c('GO','ONTOLOGY'), by.y =c('GO.ID','ONTOLOGY'))
      signature_affy_names <- select(hgu133a.db, signature_affy_ids, c("ENTREZID","GENENAME"), "PROBEID")
      enrichment_result$GO_full_results <- merge(enrichment_result$GO_full_results, signature_affy_names,
                                                 by='PROBEID')
      enrichment_result$GO_full_results <- unique(enrichment_result$GO_full_results[,c(1,4,3,2,8,5,7,8)])
      enrichment_result$GO_full_results <- enrichment_result$GO_full_results[order(enrichment_result$GO_full_results$PROBEID),]
      enrichment_result$GO_full_results_IEA_filtered <- enrichment_result$GO_full_results[!enrichment_result$GO_full_results$EVIDENCE=='IEA',]
      ########################################################################
      write.table(enrichment_result$GO_full_results_IEA_filtered, 
                  file=paste(outfolder_location,'signatures/go_terms_IEA_filtered/',signature$name,'_goterms_IEA_filtered.xls',sep=''),
                  sep='\t',row.names=FALSE,quote=FALSE)
      write.table(enrichment_result$GO_full_results, file=paste(outfolder_location,'signatures/go_terms/',signature$name,'_goterms.xls',sep=''),
                                                         sep='\t',row.names=FALSE,quote=FALSE)
    
      write.table(signature_affy_names, file=paste(outfolder_location,'signatures/affy_ids/',signature$name,'_affy_ids.xls',sep=''),
                  sep='\t',row.names=FALSE,quote=FALSE)
      enrichment_list [[signature$name]] <- enrichment_result
      # don't forget to set everything back to 0
      human_probes[intersect(names(human_probes), signature_to_affy$affy_id)] <- 0
      ######################################################################
      
      ################### connectivity map stuf ##################
      make_grp_files(signature, entrez_id__to__affy_id, conversion_table, paste(outfolder_location,'grp_files/',sep=""), cmap_ids)
      up_grp = paste(outfolder_location,'grp_files/',signature$name,'_up.grp',sep='')
      down_grp = paste(outfolder_location,'grp_files/',signature$name,'_down.grp',sep='')
      get_connectivityMap(up_grp, down_grp, signature$name, outfolder_detailed, outfolder_permuted)
      connectivity_detailed_result <- read_connectivityMap_detailed(paste(outfolder_location,'connectivity_map/detailed_results/',signature$name,'.xls',sep=""), signature$name)
      connectivity_permuted_result <- read_connectivityMap_permuted(paste(outfolder_location,'connectivity_map/permuted_results/',signature$name,'.xls',sep=""), signature$name)
      #plot_detailed_connectivity_results(connectivity_detailed_result, paste(outfolder_location,'connectivity_map/',sep=''), signature$name)
      #plot_permuted_connectivity_results(connectivity_permuted_result, paste(outfolder_location,'connectivity_map/',sep=''), signature$name)
      cmap_detailed_results[[length(cmap_detailed_results)+1]] <- connectivity_detailed_result
      cmap_permuted_results_by_name[[length(cmap_permuted_results_by_name)+1]] <- connectivity_permuted_result$by_name
      cmap_permuted_results_by_celltype[[length(cmap_permuted_results_by_celltype)+1]] <- connectivity_permuted_result$by_celltype
      cmap_permuted_results_by_ATC[[length(cmap_permuted_results_by_ATC)+1]] <- connectivity_permuted_result$by_ATC
      #############################################################
    }
    
    signature_lengths <- stack(lapply(sets_entrez, function(x) length(x)))
    if(!file.exists(paste(outfolder_location,'signature_lengths.png',sep=''))){
      #Cairo_png(filename=paste(outfolder_location,'signature_lengths.png',sep=''), width=15, height=11)
      #par(mar=c(5,18,4,2) + 0.1)
      #barplot(signature_lengths$values, names.arg=signature_lengths$ind, horiz=TRUE, las=2)
      #graphics.off()
      print(paste('figure written to', outfolder_location,'signature_lengths.png',sep=''))
    }
    print(head(enrichment_list[[1]]))
    enrichment_overlap(enrichment_list,ylab='',colnames=TRUE,
                       outfolder_location=paste(outfolder_location,'enrichment/overlap/',sep=""),
                       GO_results_name='GO_full_results')
    enrichment_overlap(enrichment_list,ylab='',colnames=TRUE,
                       outfolder_location=paste(outfolder_location,'enrichment/overlap_IEA_filtered/',sep=""),
                       'GO_full_results_IEA_filtered', main_extra_info=' - IEA_filtered')
    connectivity_map_analysis(cmap_detailed_results, merged_detailed_results, outfolder_location, cmap_permuted_results_by_name,
                              cmap_permuted_results_by_celltype, cmap_permuted_results_by_ATC)
    result_list <- list('by_ATC'=cmap_permuted_results_by_ATC, 'by_name'=cmap_permuted_results_by_name,'by_celltype'=cmap_permuted_results_by_celltype)
    drug_results <- get_drugs_list(result_list, outfolder_location=outfolder_location)
    webpage_output(drug_results, outfolder_location_root=paste(outfolder_location,'webpage_results/',sep=''), input_dataset_name=input_dataset_name)
  
  }
}

library(hgu133a.db)
library(GO.db)
library(topGO)
library(clValid)
library(limma)
library(VennDiagram)
library(gplots)
library(ggplot2)
library(XLConnect)
library(reshape2)
library(findpython)
library(ArrayExpress)
library(affy)
library(mgcv)
library(Rgraphviz)
source('GO_specificity.R')
source('enrichment.R')
source('rank_product_p.R')
load('INFUSED_microarray_data.Rdata')
source('gene_signature_similarity.R')
source('selections.R')
source('atc_info.R')
source('libraries_and_sources.R')
source('stability_validity_text.R')
source('check_mapping.R')
source('load_reference.R')
source('summarize.R')
source('connectivityMap.R')
source('locations.R')
source('validity_stability.R')
source('create_folders.R')
source('output.R')
#source('volcano_plot.R')
source('drugbank.R')
source('plots.R')
source('webpage.R')

find_drugs(INFUSED_microarray_data, infused_datalist, '/Users/NPK/Dropbox/Luxembourg/test')

