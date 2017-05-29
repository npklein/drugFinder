
get_connectivityMap <- function(up_grp, down_grp, signature_name, outfolder_detailed, outfolder_permuted){
  python_location <- find_python_cmd(minimum_version='3.0',required_modules=c('requests','re','html2text','argparse','http.cookiejar','urllib','bs4'))
  script_location <- paste(system.file(package="drugFinder"), "connectivityMap.py", sep="/")
  py_loc = paste(python_location,script_location)
  outfile_detailed = paste(outfolder_detailed,signature_name,'.xls',sep="")
  outfile_permuted = paste(outfolder_permuted,signature_name,'.xls',sep="")
  # the read.tables are to check that the file is not empty
  if((!file.exists(outfile_detailed) || !file.exists(outfile_permuted)) && length(readLines(up_grp))>0 && length(readLines(down_grp)) > 0){
     print(paste('Writing detailed connectivity map results to',outfile_detailed))
     print(paste('Writing detailed connectivity map results to',outfile_permuted))
     command <- paste(py_loc,"-u",up_grp,"-d",down_grp,"-n zebrafish -p crpsante -o",outfile_detailed,"-t",outfile_permuted)
     print(command)
     system(command)
  }
}

read_connectivityMap_detailed <- function(filename,signature_name){
  print(paste('Load workbook', filename))
  wb <- loadWorkbook(filename)
  connectivity_result <- readWorksheet(wb, 1, header = TRUE)
  connectivity_result$signature_name <- rep_len(signature_name, dim(connectivity_result)[1])
  return(connectivity_result)
}

read_connectivityMap_permuted <- function(filename, signature_name){
  wb <- loadWorkbook(filename)
  connectivity_result_byname <- readWorksheet(wb, 1, header = TRUE)
  connectivity_result_celltype <- readWorksheet(wb, 2, header = TRUE)
  connectivity_result_byATC <- readWorksheet(wb, 3, header = TRUE)
  connectivity_result_byname$signature_name <- rep_len(signature_name, dim(connectivity_result_byname)[1])
  connectivity_result_celltype$signature_name <- rep_len(signature_name, dim(connectivity_result_celltype)[1])
  connectivity_result_byATC$signature_name <- rep_len(signature_name, dim(connectivity_result_byATC)[1])
  return(list(by_name=connectivity_result_byname, by_celltype=connectivity_result_celltype, by_ATC=connectivity_result_byATC))
}

Merge <-  function(x,y) merge(x,y,by='cmap_name')

ranking_stability <- function(ranklist_df){
  ranklist_df$stability <- numeric(length=nrow(ranklist_df))
  ranking_df <- ranklist_df[,grep('.*ranking',colnames(ranklist_df))]
  no_of_columns <- dim(ranking_df)[2]
  if (no_of_columns > 2){
    x <- 1
    difference <- numeric(length=nrow(ranking_df))
    for (index in x:no_of_columns){
      column_index <- seq(no_of_columns)
      column_index <- column_index[!column_index %in% index]
      ranking_df_partial <- ranking_df[,column_index]
      # caluclate the product of each row
      ranking_df_partial_prod <- data.frame(apply(ranking_df_partial, 1, prod))
      colnames(ranking_df_partial_prod) <- c('rankprod')
      ranking_df_partial_prod$drug_name <- rownames(ranking_df_partial_prod)
      ranking_df_partial_prod <- ranking_df_partial_prod[with(ranking_df_partial_prod, order(rankprod)), ]
      ranking_df_partial_prod$rank <- 1:nrow(ranking_df_partial_prod)
      ranking_df_partial_prod$real_rank <- ranklist_df[ranking_df_partial_prod$drug_name,]$rank
      ranking_df_partial_prod$rank_difference <-ranking_df_partial_prod$real_rank - ranking_df_partial_prod$rank
      ########## VVVVVVVVV    THIS GOES WRONG        VVVVVVVVVVVV#################
      ranklist_df$stability <- ranklist_df$stability + ranking_df_partial_prod$rank_difference
      
    }
  
  ranklist_df$stability <- 0 - (ranklist_df$stability/dim(ranklist_df[,grep('.*ranking',colnames(ranklist_df))])[2])
  }else{ranklist_df$stability <- 0}
  return(ranklist_df)
}

connectivity_map_ranking <- function(cmap_permuted_results_by_name, png_name){
  # combining on ranks
  all_ranklist <- list()
  signature_names <- list()
  all_enrichment_list <- list()
  all_specificity_list <- list()
  
  for(permuted_result in cmap_permuted_results_by_name){
    signature_names[[length(signature_names)+1]] <- head(permuted_result$signature_name,1)
    ranklist <- as.list(permuted_result$rank)
    # make sure that the name is actually in the second column
    names(ranklist) <- permuted_result[,2]
    all_ranklist[[length(all_ranklist)+1]] <- ranklist
    enrichment_list <- as.list(permuted_result$enrichment)
    names(enrichment_list) <- permuted_result[,2]
    all_enrichment_list[[length(all_enrichment_list)+1]] <- enrichment_list
    specificity_list <- as.list(permuted_result$specificity)
    names(specificity_list) <- permuted_result[,2]
    all_specificity_list[[length(all_specificity_list)+1]] <- specificity_list
  }
  ranklist_df <- do.call(rbind, lapply(all_ranklist, data.frame))
  signature_names_ranklist <-  paste(signature_names,'_ranking', sep='')
  rownames(ranklist_df) <- signature_names_ranklist
  enrichment_df <- do.call(rbind, lapply(all_enrichment_list, data.frame))
  
  signature_names_enrichment <-  paste(signature_names,'_enrichment', sep='')
  rownames(enrichment_df) <- signature_names_enrichment
  enrichment_df_copy <- enrichment_df
  enrichment_df <- t(enrichment_df)
  
  specificity_df <- do.call(rbind, lapply(all_specificity_list, data.frame))
  signature_names_specificity <- paste(signature_names,'_specificity',sep='')
  rownames(specificity_df) <- signature_names_specificity
  specificity_df <- t(specificity_df)
  copy_specificity_df <- as.numeric(specificity_df)
  dim(copy_specificity_df) <- dim(specificity_df)
  dimnames(copy_specificity_df) <- dimnames(specificity_df)
  specificity_mean <- rowMeans(copy_specificity_df, na.rm=TRUE)
  
  # caluclate the product of each row
  ranklist_df_rank_prod <- apply(ranklist_df, 2, prod)
  ranklist_df_rank_prod <- data.frame(ranklist_df_rank_prod[order(ranklist_df_rank_prod)])
  colnames(ranklist_df_rank_prod) <- c('rank_prod_score')
  ranklist_df_rank_prod$p_value <- righttailgamma(ranklist_df_rank_prod$rank_prod_score, length(cmap_permuted_results_by_name), 
                                                  nrow(cmap_permuted_results_by_name[[1]]))
  ranklist_df_rank_prod <- cbind(ranklist_df_rank_prod, t(ranklist_df)[rownames(ranklist_df_rank_prod),])
  ranklist_df_rank_prod <- ranklist_df_rank_prod[order(ranklist_df_rank_prod['p_value']),]
  enrichment_df <- enrichment_df[match(rownames(ranklist_df_rank_prod),rownames(enrichment_df)),]
  ranklist_df_rank_prod <- cbind(ranklist_df_rank_prod, enrichment_df)
  specificity_df <- specificity_df[match(rownames(ranklist_df_rank_prod),rownames(specificity_df)),]
  ranklist_df_rank_prod <- cbind(ranklist_df_rank_prod, specificity_df)
  ranklist_df_rank_prod$rank <- 1:nrow(ranklist_df_rank_prod)
  ranklist_df_rank_prod$specificity_mean <- specificity_mean[rownames(ranklist_df_rank_prod)]
    ranklist_df_rank_prod <- ranking_stability(ranklist_df_rank_prod)

  enrichment_df_copy[enrichment_df_copy < 0] <- -1
  enrichment_df_copy[enrichment_df_copy > 0] <- 1
  if(!file.exists(png_name)){
    significant_drugs <- rownames(ranklist_df_rank_prod[ranklist_df_rank_prod$p_value < 0.05,])
    enrichment_df_copy <- enrichment_df_copy[,significant_drugs]
    enrichment_df_copy[ "day" ] <- rownames(enrichment_df_copy)
    enrichment_df_copy_positive <- enrichment_df_copy
    enrichment_df_copy_positive[enrichment_df_copy_positive < 0] <- 0
    enrichment_df_copy_negative <- enrichment_df_copy
    enrichment_df_copy_negative[enrichment_df_copy_negative > 0] <- 0
    enrichment_df_copy_negative[ "day" ] <- rownames(enrichment_df_copy)
    df.molten_positive <- melt( enrichment_df_copy_positive, id.vars="day", value.name="Enrichment", variable.name="Drugs" )
    colnames(df.molten_positive) <- c('day','Drugs','Enrichment')
    df.molten_negative <- melt( enrichment_df_copy_negative, id.vars="day", value.name="Enrichment", variable.name="Drugs" )
    colnames(df.molten_negative) <- c('day','Drugs','Enrichment')
  }
  ################################################
  r_length <- length(colnames(ranklist_df_rank_prod))
  ranklist_df_rank_prod <- ranklist_df_rank_prod[,c(r_length-2,2,r_length-1,r_length,3:(r_length-3),1)]
  return(ranklist_df_rank_prod)
}

connectivity_map_analysis <- function(cmap_detailed_results, merged_detailed_results, outfolder_location, cmap_permuted_results_by_name,
                                      cmap_permuted_results_by_celltype, cmap_permuted_results_by_ATC){
  

  # combined on p-value stuff
  merged_detailed_results <- do.call("rbind", cmap_detailed_results)
  merged_permuted_results_by_name <- do.call("rbind",cmap_permuted_results_by_name)
  merged_permuted_results_by_name_filtered <- merged_permuted_results_by_name[!merged_permuted_results_by_name$p=='---',]
  permuted_results_by_name_ordered_pvalue <- merged_permuted_results_by_name_filtered[order(merged_permuted_results_by_name_filtered$p),]
  permuted_results_by_name_unduplicate <- permuted_results_by_name_ordered_pvalue[!duplicated(permuted_results_by_name_ordered_pvalue$cmap.name),]
  write.table(permuted_results_by_name_ordered_pvalue, file=paste(outfolder_location,'connectivity_map/permuted_results_by_name.xls',sep=""),
              quote=FALSE, row.names=FALSE,sep='\t')
  write.table(permuted_results_by_name_unduplicate, file=paste(outfolder_location,'connectivity_map/permuted_results_by_name_unduplicate.xls',sep=""),
              quote=FALSE, row.names=FALSE,sep='\t')

  merged_permuted_results_by_celltype <- do.call("rbind",cmap_permuted_results_by_celltype)
  merged_permuted_results_by_celltype_filtered <- merged_permuted_results_by_celltype[!merged_permuted_results_by_celltype$p=='---',]
  permuted_results_by_celltype_ordered_pvalue <- merged_permuted_results_by_celltype_filtered[order(merged_permuted_results_by_celltype_filtered$p),]
  permuted_results_by_celltype_unduplicate <- permuted_results_by_celltype_ordered_pvalue[!duplicated(permuted_results_by_celltype_ordered_pvalue$cmap.name.and.cell.line),]
  write.table(permuted_results_by_celltype_ordered_pvalue, file=paste(outfolder_location,'connectivity_map/permuted_results_by_celltype.xls',sep=""),
              quote=FALSE, row.names=FALSE,sep='\t')
  write.table(permuted_results_by_celltype_unduplicate, file=paste(outfolder_location,'connectivity_map/permuted_results_by_celltype_unduplicate.xls',sep=""),
              quote=FALSE, row.names=FALSE,sep='\t')

  merged_permuted_results_by_ATC <- do.call("rbind",cmap_permuted_results_by_ATC)
  merged_permuted_results_by_ATC_filtered <- merged_permuted_results_by_ATC[!merged_permuted_results_by_ATC$p=='---',]
  permuted_results_by_ATC_ordered_pvalue <- merged_permuted_results_by_ATC_filtered[order(merged_permuted_results_by_ATC_filtered$p),]
  permuted_results_by_ATC_unduplicate <- permuted_results_by_ATC_ordered_pvalue[!duplicated(permuted_results_by_ATC_ordered_pvalue$atc.code),]
  write.table(permuted_results_by_ATC_ordered_pvalue, file=paste(outfolder_location,'connectivity_map/permuted_results_by_ATC.xls',sep=""),
              quote=FALSE, row.names=FALSE,sep='\t')
  write.table(permuted_results_by_ATC_unduplicate, file=paste(outfolder_location,'connectivity_map/permuted_results_by_ATC_unduplicate.xls',sep=""),
              quote=FALSE, row.names=FALSE,sep='\t')
  
}


get_drugs_list <- function(cmap_permuted_results, outfolder_location){
  atc_list <- list()
  for (permuted_result in cmap_permuted_results[['by_ATC']]){
    atc_list <- append(atc_list, permuted_result$atc.code)
  }
  atc_list <- unique(unlist(atc_list))
  atc_list_file <- paste(outfolder_location,'candidate_drugs/atc_codes.txt',sep='')
  write(atc_list, file = atc_list_file,sep = '\n')
  atc_info <- get_atc_info(atc_list_file, paste(outfolder_location,'candidate_drugs/atc_codes_full_names.txt',sep=''),
               paste(outfolder_location,'candidate_drugs/atc_codes_descriptions.txt',sep=''))
  result_list <- list()
  for (permuted_result_type in names(cmap_permuted_results)){
    outfolder_location_result_type <- paste(outfolder_location,'candidate_drugs/',permuted_result_type,'/',sep='')
    cmap_permuted_results_by_negative_enrichment <- lapply(cmap_permuted_results[[permuted_result_type]], function(df){
      df[order(df$enrichment),]
    })
    for( i in seq_along(cmap_permuted_results_by_negative_enrichment)){
      cmap_permuted_results_by_negative_enrichment[[i]]$rank <- 1:nrow(cmap_permuted_results_by_negative_enrichment[[i]])
    }
    cmap_permuted_results_positive_enrichment <- lapply(cmap_permuted_results[[permuted_result_type]], function(df){
      df[order(df$enrichment, decreasing=TRUE),]
    })
    for( i in seq_along(cmap_permuted_results_positive_enrichment)){
      cmap_permuted_results_positive_enrichment[[i]]$rank <- 1:nrow(cmap_permuted_results_positive_enrichment[[i]])
    }
    rankscores_by_positive_enrichment <- connectivity_map_ranking(cmap_permuted_results_positive_enrichment, png_name=paste(outfolder_location_result_type, 'drug_enrichment_scores_by_positive_enrichment.png',sep=''))
    rankscores_by_pvalue <- connectivity_map_ranking(cmap_permuted_results[[permuted_result_type]], png_name=paste(outfolder_location_result_type, 'drug_enrichment_scores_by_pvalue.png',sep=''))
    rankscores_by_negative_enrichment <- connectivity_map_ranking(cmap_permuted_results_by_negative_enrichment, png_name=paste(outfolder_location_result_type, 'drug_enrichment_scores_by_negative_enrichment.png',sep=''))
    if (permuted_result_type == 'by_ATC'){
      rankscores_by_positive_enrichment$drug_names <- character(length=dim(rankscores_by_positive_enrichment)[1])
      rankscores_by_pvalue$drug_names <- character(length=dim(rankscores_by_pvalue)[1])
      rankscores_by_negative_enrichment$drug_names <- character(length=dim(rankscores_by_negative_enrichment)[1])
      rankscores_by_positive_enrichment$description <- character(length=dim(rankscores_by_positive_enrichment)[1])
      rankscores_by_pvalue$description <- character(length=dim(rankscores_by_pvalue)[1])
      rankscores_by_negative_enrichment$description <- character(length=dim(rankscores_by_negative_enrichment)[1])
      for(atc_code in unique(c(rownames(rankscores_by_positive_enrichment), 
                               rownames(rankscores_by_negative_enrichment),
                               rownames(rankscores_by_pvalue)))){
        rankscores_by_positive_enrichment[atc_code,]$drug_names <- paste(atc_info$full_name[atc_info[['full_name']]$atc_code==atc_code,]$full_name,collapse=', ')
        rankscores_by_pvalue[atc_code,]$drug_names <- paste(atc_info$full_name[atc_info[['full_name']]$atc_code==atc_code,]$full_name,collapse=', ')
        rankscores_by_negative_enrichment[atc_code,]$drug_names <- paste(atc_info$full_name[atc_info[['full_name']]$atc_code==atc_code,]$full_name,collapse=', ')
        rankscores_by_positive_enrichment[atc_code,]$description <- paste(atc_info$info[atc_info[['info']]$atc_code==atc_code,]$partial_code, atc_info$info[atc_info[['info']]$atc_code==atc_code,]$description, collapse=', ')
        rankscores_by_pvalue[atc_code,]$description <- paste(atc_info$info[atc_info[['info']]$atc_code==atc_code,]$partial_code, atc_info$info[atc_info[['info']]$atc_code==atc_code,]$description, collapse=', ')
        rankscores_by_negative_enrichment[atc_code,]$description <- paste(atc_info$info[atc_info[['info']]$atc_code==atc_code,]$partial_code, atc_info$info[atc_info[['info']]$atc_code==atc_code,]$description, collapse=', ')
        combined_results_positive <- result_list$positive_by_name[,c(1,2,3,4)]
      }
      r_length <- length(colnames(rankscores_by_positive_enrichment))
      rankscores_by_positive_enrichment <- rankscores_by_positive_enrichment[,c(1:4,r_length-1, r_length,5:(r_length-2))]
      atc_results_positive <- rankscores_by_positive_enrichment
      atc_results_negative <- rankscores_by_negative_enrichment
    }
    else if (permuted_result_type == 'by_name'){
      name_results_positive <- rankscores_by_positive_enrichment
      name_results_negative <- rankscores_by_negative_enrichment
    }
    write.table(rankscores_by_pvalue, file=paste(outfolder_location_result_type,'drug_results_by_pvalue.xls',sep=''), quote=FALSE,sep="\t",
                row.names=TRUE, col.names=NA)
    write.table(rankscores_by_negative_enrichment, file=paste(outfolder_location_result_type,'drug_results_by_negative_enrichment.xls',sep=''), quote=FALSE,sep="\t",
                row.names=TRUE, col.names=NA)
    write.table(rankscores_by_positive_enrichment, file=paste(outfolder_location_result_type,'drug_results_by_positive_enrichment.xls',sep=''), quote=FALSE,sep="\t",
                row.names=TRUE, col.names=NA)
    result_list[[paste('positive_', permuted_result_type, sep='')]] <- rankscores_by_positive_enrichment
    result_list[[paste('negative_', permuted_result_type, sep='')]] <- rankscores_by_negative_enrichment
    result_list[[paste('pvalue_', permuted_result_type, sep='')]] <- rankscores_by_pvalue
  }
  

  combined_results_positive <- result_list$positive_by_name[,c(1,2,3,4)]
  combined_results_negative <- result_list$negative_by_name[,c(1,2,3,4)]
  combined_results_positive$atc_codes <- character(length=dim(combined_results_positive)[1])
  combined_results_negative$atc_codes <- character(length=dim(combined_results_negative)[1])
  combined_results_positive$atc_rankings <- character(length=dim(combined_results_positive)[1])
  combined_results_negative$atc_rankings <- character(length=dim(combined_results_negative)[1])
  for (drug_name in unique(c(rownames(combined_results_positive),
                         rownames(combined_results_negative)))){
    atc_codes <- atc_info[['full_name']][atc_info[['full_name']]$full_name==drug_name,]$atc_code
    combined_results_positive[drug_name,]$atc_codes <- paste(atc_codes, collapse=', ')
    combined_results_negative[drug_name,]$atc_codes <- paste(atc_codes, collapse=', ')
    combined_results_positive[drug_name,]$atc_rankings <- paste(result_list$positive_by_ATC[as.character(atc_codes),]$rank, collapse=', ')
    combined_results_negative[drug_name,]$atc_rankings <- paste(result_list$negative_by_ATC[as.character(atc_codes),]$rank, collapse=', ')    
  }
  write.table(combined_results_positive, file=paste(outfolder_location,'candidate_drugs/summarized_results_positive_enrichment.xls',sep=''), 
              quote=FALSE,sep="\t",row.names=TRUE, col.names=NA)
  write.table(combined_results_negative, file=paste(outfolder_location,'candidate_drugs/summarized_results_negative_enrichment.xls',sep=''), 
              quote=FALSE,sep="\t",row.names=TRUE, col.names=NA)
  return(list('combined_results_positive'=combined_results_positive, 'combined_results_negative'=combined_results_negative,
         'atc_results_positive'=atc_results_positive, 'atc_results_negative'=atc_results_negative,
         'name_results_positive'=name_results_positive, 'name_results_negative'=name_results_negative))
}




