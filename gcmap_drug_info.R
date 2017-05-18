gcmap_non_directional <- function(cmap_data, zebrafishSel_id, name1, name2){
  egs <- GeneSet(unique(zebrafishSel_id),setName = "Sample")
  res <- fisher_score( egs, cmap_data$sets, universe=featureNames( cmap_data$sets))
  png_name = paste(outfolder_location,'fisher_exact_results/', name1,"__",name2,".png",sep="")
  png(png_name,type="cairo",units="in", width=7, height=6, pointsize=12, res=300)
  plot(res)
  dev.off()
  file_name <- paste(outfolder_location,"Drugs/",name1,"__",name2,".txt",sep="")
  if(!file.exists(file_name)){
    fileConn<-file(file_name)
    write.table(cmapTable(res), fileConn, quote=FALSE, row.names=FALSE, sep='\t')
    print(paste('Writing drugs to ', file_name))
  }
}