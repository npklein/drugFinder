library(hgu133a.db)
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
  t <- find_python_cmd(minimum_version='3.0',required_modules=c('requests','html2text','argparse','urllib','bs4'))
  if (substr(outfolder_location_root, nchar(outfolder_location_root), nchar(outfolder_location_root))!='/'){outfolder_location_root <-paste(outfolder_location_root,'/',sep='')}
  memory.limit(as.numeric(memory_limit))
  dir.create(outfolder_location_root, showWarnings = FALSE)
  rownames(microarray_data) <- make.names(microarray_data[,'Gene.Symbol'], unique=TRUE)
  conversion_table <- conversion_table[,c('Gene.Symbol','entrez')]
  signatures <- list()
  signature_size <- list()
  for (input_dataset_name in names(datasets)){
    print(paste('Processing group',input_dataset_name))
    outfolder_location <- paste(outfolder_location_root,gsub('\\s','', input_dataset_name),'/',sep='')
    print('Creating dir structure')
    create_folders(outfolder_location)
    for (input_data in datasets[[input_dataset_name]]){
      name1 = input_data$name1
      name2_original = input_data$name2
      print('Selecting genes for signature')
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
        signatures[[paste(name1,'__',all_locations$name2,sep='')]] <- list(gene_list=gene_list,gene_ids=geneSel_id, 
                                                            name=paste(name1,"__",all_locations$name2,sep=""),
                                                            gene_list_down=gene_list_down,gene_list_up=gene_list_up,
                                                            gene_list_complete=selection_list$gene_list)
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
      signature_affy_names <- select(hgu133a.db, signature_affy_ids, c("ENTREZID","GENENAME"), "PROBEID")
      # don't forget to set everything back to 0
      human_probes[intersect(names(human_probes), signature_to_affy$affy_id)] <- 0
      
      ################### connectivity map stuf ##################
      print('starting connectivity map')
      make_grp_files(signature, entrez_id__to__affy_id, conversion_table, paste(outfolder_location,'grp_files/',sep=""), cmap_ids)
      up_grp = paste(outfolder_location,'grp_files/',signature$name,'_up.grp',sep='')
      down_grp = paste(outfolder_location,'grp_files/',signature$name,'_down.grp',sep='')
      get_connectivityMap(up_grp, down_grp, signature$name, outfolder_detailed, outfolder_permuted)
      connectivity_result <- paste0(outfolder_location,'connectivity_map/detailed_results/',signature$name,'.xls')
      connectivity_result_permuted <- paste0(outfolder_location,'connectivity_map/permuted_results/',signature$name,'.xls')
      print(paste('Connectivity map done, reading results from',connectivity_result,'and',connectivity_result_permuted))
      connectivity_detailed_result <- read_connectivityMap_detailed(connectivity_result, signature$name)
      print('Reading detailed result done')
      connectivity_permuted_result <- read_connectivityMap_permuted(connectivity_result_permuted, signature$name)
      print('Reading permuted result done')

      cmap_detailed_results[[length(cmap_detailed_results)+1]] <- connectivity_detailed_result
      cmap_permuted_results_by_name[[length(cmap_permuted_results_by_name)+1]] <- connectivity_permuted_result$by_name
      cmap_permuted_results_by_celltype[[length(cmap_permuted_results_by_celltype)+1]] <- connectivity_permuted_result$by_celltype
      cmap_permuted_results_by_ATC[[length(cmap_permuted_results_by_ATC)+1]] <- connectivity_permuted_result$by_ATC
      #############################################################
    }
    

    connectivity_map_analysis(cmap_detailed_results, merged_detailed_results, outfolder_location, cmap_permuted_results_by_name,
                              cmap_permuted_results_by_celltype, cmap_permuted_results_by_ATC)
    result_list <- list('by_ATC'=cmap_permuted_results_by_ATC, 'by_name'=cmap_permuted_results_by_name,'by_celltype'=cmap_permuted_results_by_celltype)
    drug_results <- get_drugs_list(result_list, outfolder_location=outfolder_location)
    webpage_output(drug_results, outfolder_location_root=paste(outfolder_location,'webpage_results/',sep=''), input_dataset_name=input_dataset_name)
  
  }
}

