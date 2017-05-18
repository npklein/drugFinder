output_location <- function(selection_method, name2_original, outfolder_location){
  if(selection_method=='half_strict'){
    name2 <- paste(name2_original, "_strict_15fold", sep="")
    dendogram_location_samples <- paste(outfolder_location,"strict_15fold_samples_dendrogram/",sep="")
    dir.create(dendogram_location_samples, showWarnings = FALSE)
    dendogram_location_genes <- paste(outfolder_location,"strict_15fold_genes_dendrogram/",sep="")
    dir.create(dendogram_location_genes, showWarnings = FALSE)
    heatmap_location <- paste(outfolder_location,"strict_15fold_heatmap_",sep="")
  }
  else if(selection_method=='strict'){
    name2 <- paste(name2_original, "_strict", sep="")
    dendogram_location_samples <- paste(outfolder_location,"strict_samples_dendrogram/",sep="")
    dir.create(dendogram_location_samples, showWarnings = FALSE)
    dendogram_location_genes <- paste(outfolder_location,"strict_genes_dendrogram/",sep="")
    dir.create(dendogram_location_genes, showWarnings = FALSE)
    heatmap_location <- paste(outfolder_location,"strict_heatmap_",sep="")
  }
  else if(selection_method=='relaxed'){
    name2 <-name2_original
    volcano_location <- paste(outfolder_location,"Volcano/",sep="")
    dir.create(volcano_location, showWarnings = FALSE)
    dendogram_location_samples <- paste(outfolder_location,"samples_dendrogram/",sep="")
    dir.create(dendogram_location_samples, showWarnings = FALSE)
    dendogram_location_genes <- paste(outfolder_location,"genes_dendrogram/",sep="")
    dir.create(dendogram_location_genes, showWarnings = FALSE)
    heatmap_location <- paste(outfolder_location,"heatmap_",sep="")
  }
  else{
    stop("This should not be possible")
  }
  return(list(name2=name2, dendogram_location_samples=dendogram_location_samples,
              dendogram_location_genes=dendogram_location_genes,heatmap_location=heatmap_location,
              volcano_location=paste(outfolder_location,'Volcano/',sep="")))
}