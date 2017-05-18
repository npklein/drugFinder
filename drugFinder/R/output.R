make_grp_files <- function(signature, entrez_id__to__affy_id, conversion_table, outfolder, cmap_ids){
  dir.create(outfolder, showWarnings = FALSE)

  # ordered from high to low
  gene_list_up_converted <- conversion_table[match(toupper(rownames(signature$gene_list_up)), toupper(conversion_table$Gene.Symbol)),]
  gene_list_down_converted <- conversion_table[match(toupper(rownames(signature$gene_list_down)), toupper(conversion_table$Gene.Symbol)),]
  gene_affy_ids <- entrez_id__to__affy_id[entrez_id__to__affy_id$entrez_id %in% signature$gene_ids,]
  
  
  # conversion
  up_probes <- gene_affy_ids[gene_affy_ids$entrez_id %in% gene_list_up_converted$entrez,]
  down_probes <- gene_affy_ids[gene_affy_ids$entrez_id %in% gene_list_down_converted$entrez,]
  # order them in same order as gene_list_up_converted and gene_list_down_converted
  up_probes_ordered <- up_probes[order(match(up_probes$entrez_id,gene_list_up_converted$entrez)),]
  down_probes_ordered <- down_probes[order(match(down_probes$entrez_id,gene_list_down_converted$entrez)),]
  # because 2 zebrafish probes can map to the same human probe, some human probes can be found in both 
  # up and down list. Because connectivity map does not accept this, remove these values (from both lists)
  up_probes_ordered <- up_probes_ordered[!up_probes_ordered$affy_id %in% down_probes_ordered$affy_id,]
  down_probes_ordered <- down_probes_ordered[!down_probes_ordered$affy_id %in% up_probes_ordered$affy_id,]
  # filter on only the once that are present in rankMatrix (the ones that are found in Connectivity Map)
  up_probes_filtered <- up_probes_ordered[up_probes_ordered$affy_id %in% cmap_ids,]
  down_probes_filtered <- down_probes_ordered[down_probes_ordered$affy_id %in% cmap_ids,]
  # only unique affy id
  up_probes_filtered_unique <- up_probes_filtered[!duplicated(up_probes_filtered$affy_id),]
  down_probes_filtered_unique <- down_probes_filtered[!duplicated(down_probes_filtered$affy_id),]
  if(dim(up_probes_filtered_unique)[1]+dim(down_probes_filtered_unique)[1] > 1000){
    # can only have max 1000 tags. Remove till 1000 left over, depending on difference in size between up and down
    to_remove <- dim(up_probes_filtered_unique)[1]+dim(down_probes_filtered_unique)[1] - 1000
    remove_factor <- (dim(up_probes_filtered)[1] / dim(down_probes_filtered)[1])
    remove_up <- round(to_remove*remove_factor)
    remove_down <- round(to_remove*(1-remove_factor))
    up_probes_filtered_unique <- up_probes_filtered_unique[1:(dim(up_probes_filtered_unique)[1]-remove_up),]
    down_probes_filtered_unique <- down_probes_filtered_unique[1:(dim(down_probes_filtered_unique)[1]-remove_down),]
  }
  # outfile writing
  up_list_name = paste(outfolder,signature$name,'_up.grp',sep='')
  down_list_name = paste(outfolder,signature$name,'_down.grp',sep='')
  if(!file.exists(up_list_name)){
    print(paste('uplist written to',up_list_name))
    write.table(up_probes_filtered_unique$affy_id,up_list_name,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
  if(!file.exists(down_list_name)){
    print(paste('downlist written to',down_list_name))
    write.table(down_probes_filtered_unique$affy_id,down_list_name,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
