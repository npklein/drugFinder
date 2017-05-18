#' Check how many zebrafish affymatrix probes are mapped to zebrafish genes
#' 
#' Checks the loaded conversion table for how well the conversion is done, and by which 
#' source the conversion is done.
#' @param microarray_data The microarray input data
#' @param control_desired_factor Factor outlining which samples are control and which are desired expression state
check_mapping <- function(conversion_table,outfolder_location){
  if(!file.exists(paste(outfolder_location,"zebra_to_human_mapping.emf",sep=''))){
    entrez_from_symbol <- conversion_table[!is.na(conversion_table$entrez_from_symbol),]$Gene.Symbol
    entrez_from_mart <- conversion_table[!is.na(conversion_table$entrez_from_mart),]$Gene.Symbol
    entrez_from_blast <- conversion_table[!is.na(conversion_table$entrez_from_blast),]$Gene.Symbol
    entrez_from_zfin <- conversion_table[!is.na(conversion_table$entrez_from_zfin),]$Gene.Symbol
    entrez_from_arnaud <- conversion_table[!is.na(conversion_table$entrez_from_arnaud),]$Gene.Symbol
    entrez_from_homologene <- conversion_table[!is.na(conversion_table$entrez_from_homologene),]$Gene.Symbol
    not_mapped <- length(conversion_table[!is.na(conversion_table$entrez),]$Gene.Symbol)
    total <- length(conversion_table$Gene.Symbol)
    
   # venn.diagram(list(homologene=entrez_from_homologene, mart = entrez_from_symbol, 
   #                   blast = entrez_from_blast, zfin = entrez_from_zfin, arnaud=entrez_from_arnaud),
  #               fill = c("red", "green","blue","yellow", "purple"),
  #               alpha = c(0.5, 0.5, 0.5,0.5, 0.5), cex = 1,cat.fontface = 4,lty =1, fontfamily =3, 
  #               filename = paste(outfolder_location,"zebra_to_human_mapping.emf",sep=''),
  #               main=paste(not_mapped,'/',total,'genes mapped (',(not_mapped/total)*100,'%)', sep=''))
  #print(paste('png written to ',outfolder_location,'zebra_to_human_mapping.emf',sep=''))
  }
}