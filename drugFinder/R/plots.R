#' Plot information on the signatures
#' 
#' Plot a lot of information on the signatures. This includes heatmaps (euclidian and correlation distance),
#' sample and gene dendograms and volcano plots
#' @param name1 Name of the desired state samples of the signature
#' @param name2 Name of the control samples of the signature
#' @param geneSel The selected genes
#' @param heatmap_location Folder location to write heatmap 
#' @param dendogram_location_samples Folder location to write dendogram
#' @param volcano_location Folder location to write volcano plots
#' @param f Definitition of which sample is control and which is 'treatment'
#' @param fit The fitting object from when the differntial genes were selected
#' 
plot_results <- function(name1, name2, geneSel, heatmap_location, dendogram_location_samples,
                         dendogram_location_genes, volcano_location, f, fit){
  dir.create(paste(heatmap_location, "euclidean/", sep=""), showWarnings = FALSE)
  dir.create(paste(heatmap_location, "correlation/", sep=""), showWarnings = FALSE)
  color.map <- function(f) { if (f=="FIRST") "#FF0000" else if(f=="THIRD") "#00FF00" else "#0000FF" }
  heatmap_colors <- unlist(lapply(f, color.map))
  #### heatmap euclidean ####
  png_name = paste(heatmap_location, "euclidean/",name1,"__",name2,".png",sep="")
  validity_euclidean <- validity(geneSel, metric="euclidean")
  stability_euclidean <- stability(geneSel, metric="euclidean")
    annotated_heatmap(geneSel=geneSel, heatmap_colors=heatmap_colors, name1=name1, name2=name2, 
                      metric="euclidean", clust_validity=validity_euclidean, clust_stability=stability_euclidean,
                      png_name=png_name)
    print(paste('png written to',png_name))
  #else{print(paste(png_name,"already exists"))}
  
  #### heatmap correlation ####
  stability_pearson <- stability(geneSel, metric="correlation")
  validity_pearson <- validity(geneSel, metric="correlation")
  png_name = paste(heatmap_location, "correlation/",name1,"__",name2,".png", sep="")
  #annotated_heatmap(geneSel=geneSel, heatmap_colors=heatmap_colors, name1=name1, name2=name2, 
  #                    metric="correlation", clust_validity=validity_pearson, clust_stability=stability_pearson,
  #                    png_name=png_name) 
  #  print(paste('png written to',png_name))
  #else{print(paste(png_name,"already exists"))}
  
  #### separate samples dendrogram ####
  png_name = paste(dendogram_location_samples,name1,"__",name2,"_euclidian",".png",sep="")
  #  genes_samples_dendogram(geneSel=geneSel, name1=name1, name2=name2, clust_validity=validity_euclidean, 
  #                          clust_stability=stability_euclidean, metric='euclidian', gene_or_sample='samples',
  #                          location=dendogram_location_samples, png_name=png_name)
  #  print(paste('png written to',png_name))
  #else{print(paste(png_name,"already exists"))}
  #png_name = paste(dendogram_location_samples,name1,"__",name2,"_correlation",".png",sep="")
  #  genes_samples_dendogram(geneSel=geneSel, name1=name1, name2=name2, clust_validity=validity_pearson, 
  #                          clust_stability=stability_pearson, metric='correlation', gene_or_sample='samples',
  #                          location=dendogram_location_samples, png_name=png_name)
  #  print(paste('png written to ',png_name))
  #else{print(paste(png_name,"already exists"))}
  
  #### separate genes dendogram ####
  #png_name = paste(dendogram_location_genes,name1,"__",name2,"_euclidian",".png",sep="")
  #  genes_samples_dendogram(geneSel=geneSel, name1=name1, name2=name2, clust_validity=validity_euclidean, 
  #                          clust_stability=stability_euclidean, metric='euclidian', gene_or_sample='genes',
  #                          location=dendogram_location_genes, png_name=png_name)
  #else{print(paste(png_name,"already exists"))}
  #png_name = paste(dendogram_location_genes,name1,"__",name2,"_correlation",".png",sep="")
  #  genes_samples_dendogram(geneSel=geneSel, name1=name1, name2=name2, clust_validity=validity_pearson, 
  #                          clust_stability=stability_pearson, metric='correlation', gene_or_sample='genes',
  #                          location=dendogram_location_genes, png_name=png_name)
  #else{print(paste(png_name,"already exists"))}
  
  #### volcano plot ####
  #png_name = paste(volcano_location, name1,"__",name2,".png",sep="")
  #  volcano_plot(fit, name1, name2,location=volcano_location, png_name=png_name)
  
}

#' Plot an heatmap
#' 
#' Plot an heatmap
#' @param geneSel The selected genes
#' @param heatmap_colors Colors to use for the heatmap
#' @param name1 Name of the desired state samples of the signature
#' @param name2 Name of the control samples of the signature
#' @param metric Distance metric (euclidian or correlation)
#' @param clust_validity Cluster validity scores
#' @param f clust_stability cluster stability scores
#' @param png_name name of the png to write
#' 
annotated_heatmap <- function(geneSel, heatmap_colors, name1, name2, metric, clust_validity,
                              clust_stability, png_name){
  print(paste("Writing heatmap to ", png_name))
  par(mar=c(9.5,3,0.5, 0.3)) 
  #Cairo_png(filename=png_name, width=13, height=11)
  if(metric=="correlation"){
    heatmap.2(geneSel, col=redgreen(75), scale="row", ColSideColors=heatmap_colors,
              key=TRUE, symkey=FALSE, density.info="none",cexRow=1,cexCol=1,margins=c(6,11),
              trace="none",srtCol=45, Rowv=TRUE, main=paste(name1,"vs",name2),
              hclustfun=function(geneSel) hclust(geneSel,method="complete"),
              distfun=function(geneSel) as.dist((1-cor(t(geneSel)))/2))
  }
  else if (metric=="euclidean"){
    heatmap.2(geneSel, col=redgreen(75), scale="row", ColSideColors=heatmap_colors,
              key=TRUE, symkey=FALSE, density.info="none",cexRow=1,cexCol=1,margins=c(6,11),
              trace="none",srtCol=45, Rowv=TRUE, main=paste(name1,"vs",name2))    
  }
  else{stop(paste(metric," metric for heatmap not implemented yet"))}
  add_stability_validity_text(clust_validity=clust_validity, clust_stability=clust_stability)
  graphics.off()
}

#' Plot results from the detailed connectivity results page
#' 
#' <todo>
#' 
plot_detailed_connectivity_results <- function(connectivity_data_frame, outfolder, signature_name){
  outfile <- paste(outfolder,'scores/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)
    #plot(connectivity_data_frame$score[order(connectivity_data_frame$score,decreasing=TRUE)], xlab='Instances',ylab='Connectivity score',main=signature_name)   
    #graphics.off()
    #print(paste('png written to',outfile))
  
  dir.create(paste(outfolder,'dose_vs_scores/',sep=""), showWarnings = FALSE)
  outfile <- paste(outfolder,'dose_vs_scores/', signature_name,'.png', sep="")
    #Cairo_png(filename=outfile, width=13, height=11)                    
    #plot(as.numeric(gsub("\\D", "", connectivity_data_frame$dose)), connectivity_data_frame$score,
    #     xlab='Dose in micro Liter', ylab='Connectivity score', main=signature_name)
    #graphics.off()
    print(paste('png written to',outfile))
  
  outfile <- paste(outfolder,'up_vs_down/', signature_name,'.png', sep="")
    #Cairo_png(filename=outfile, width=13, height=11)                    
    #plot(connectivity_data_frame$down, connectivity_data_frame$up,
    #     xlab='up score', ylab='down score', main=signature_name)
    #graphics.off()
    print(paste('png written to',outfile))
}

#' Plot results from the permuted connectivity results page
#' 
#' <todo>
#' 
plot_permuted_connectivity_results <- function(connectivity_data_frame, outfolder, signature_name){
  for (result_type in names(connectivity_data_frame)){
    p_value_filtered <- connectivity_data_frame[[result_type]][connectivity_data_frame[[result_type]]$p < 0.05,]
    p_value_filtered <- p_value_filtered[!p_value_filtered$p=='---',]
    result_type_folder <- result_type
    if (result_type == 'merged'){result_type_folder <- ''}
    dir.create(paste(outfolder,result_type_folder,'/mean/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/mean/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)
      #plot(connectivity_data_frame[[result_type]]$mean[order(connectivity_data_frame[[result_type]]$mean,decreasing=TRUE)], xlab='Instances',
      #     ylab='Mean connectivity score',main=signature_name)
      #graphics.off()
      print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/enrichment/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/enrichment/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(connectivity_data_frame[[result_type]]$enrichment[order(connectivity_data_frame[[result_type]]$enrichment)], xlab='Instances', ylab='Enrichment', main=signature_name)
      #graphics.off()
      print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/p_value/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/p_value/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(unique(connectivity_data_frame[[result_type]]$p[order(connectivity_data_frame[[result_type]]$p)]), xlab='Instances', ylab='p-value', main=signature_name)
      #graphics.off()
      print(paste('png written to',outfile))
    dir.create(paste(outfolder,result_type_folder,'/specificity/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/specificity/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(unique(connectivity_data_frame[[result_type]]$specificity[order(connectivity_data_frame[[result_type]]$specificity)]), xlab='Instances', 
      #     ylab='specificity', main=signature_name)
      #graphics.off()
      print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/non_null/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/non_null/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(connectivity_data_frame[[result_type]]$percent.non.null[order(connectivity_data_frame[[result_type]]$percent.non.null)],
      #     xlab='Instances', ylab='% non null', main=signature_name)
      #graphics.off()
      print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/mean_vs_enrichment/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/mean_vs_enrichment/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(connectivity_data_frame[[result_type]]$mean, connectivity_data_frame[[result_type]]$enrichment,
      #     xlab='Mean connectivity score', ylab='Enrichment', main=signature_name)
      #graphics.off()
      print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/enrichment_vs_p_value/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/enrichment_vs_p_value/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(connectivity_data_frame[[result_type]]$enrichment, connectivity_data_frame[[result_type]]$p,
      #     xlab='Enrichment', ylab='p-value', main=signature_name)
      #graphics.off()
      print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/p_value_vs_specificity/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/p_value_vs_specificity/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #try(plot(connectivity_data_frame[[result_type]]$p, connectivity_data_frame[[result_type]]$specificity,
      #     xlab='p-value', ylab='specificity', main=signature_name))
      #graphics.off()
      print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/mean__pvalue_filtered/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/mean__pvalue_filtered/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)
      #plot(p_value_filtered$mean[order(p_value_filtered$mean,decreasing=TRUE)], xlab='Instances',
      #     ylab='Mean connectivity score',main=signature_name)
      #graphics.off()
      #print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/enrichment__pvalue_filtered/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/enrichment__pvalue_filtered/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(p_value_filtered$enrichment[order(p_value_filtered$enrichment)], xlab='Instances', ylab='Enrichment', main=signature_name)
      #graphics.off()
      #print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/p_value__pvalue_filtered/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/p_value__pvalue_filtered/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(unique(p_value_filtered$p[order(p_value_filtered$p)]), xlab='Instances', ylab='p-value', main=signature_name)
      #graphics.off()
      #print(paste('png written to',outfile))
    dir.create(paste(outfolder,result_type_folder,'/specificity__pvalue_filtered/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/specificity__pvalue_filtered/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(unique(p_value_filtered$specificity[order(p_value_filtered$specificity)]), xlab='Instances', 
      #     ylab='specificity', main=signature_name)
      #graphics.off()
      #print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/non_null__pvalue_filtered/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/non_null__pvalue_filtered/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(p_value_filtered$percent.non.null[order(p_value_filtered$percent.non.null)],
      #     xlab='Instances', ylab='% non null', main=signature_name)
      #graphics.off()
      #print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/mean_vs_enrichment__pvalue_filtered/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/mean_vs_enrichment__pvalue_filtered/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(p_value_filtered$mean, p_value_filtered$enrichment,
      #     xlab='Mean connectivity score', ylab='Enrichment', main=signature_name)
      #graphics.off()
      #print(paste('png written to',outfile))
    
    dir.create(paste(outfolder,result_type_folder,'/enrichment_vs_p_value__pvalue_filtered/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/enrichment_vs_p_value__pvalue_filtered/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)
      #plot(p_value_filtered$enrichment, p_value_filtered$p,
      #     xlab='Enrichment', ylab='p-value', main=signature_name)
      #graphics.off()
      #print(paste('png written to',outfile))
      
    
    dir.create(paste(outfolder,result_type_folder,'/p_value_vs_specificity__pvalue_filtered/',sep=""), showWarnings = FALSE)
    outfile <- paste(outfolder,result_type_folder,'/p_value_vs_specificity__pvalue_filtered/', signature_name,'.png', sep="")
      #Cairo_png(filename=outfile, width=13, height=11)                    
      #plot(p_value_filtered$p, p_value_filtered$specificity,
      #     xlab='p-value', ylab='specificity', main=signature_name)
      #graphics.off()
      #print(paste('png written to',outfile))
    graphics.off()
  }
}


plot_total_connectivity_results <- function(connectivity_data_frame, outfolder, signature_name){
  p_value_filtered <- connectivity_data_frame[connectivity_data_frame$p < 0.05,]
  p_value_filtered <- p_value_filtered[!p_value_filtered$p=='---',]
  outfile <- paste(outfolder,'score_vs_mean_score/', signature_name,'.png', sep="")
    #Cairo_png(filename=outfile, width=13, height=11)
    #plot(connectivity_data_frame$score, connectivity_data_frame$mean, xlab='score',
    #     ylab='Mean connectivity score',main=signature_name)
    #graphics.off()
    print(paste('png written to',outfile))
  
  dir.create(paste(outfolder,'score_vs_mean_score__p_value_filtered/',sep=""), showWarnings = FALSE)
  outfile <- paste(outfolder,'score_vs_mean_score__p_value_filtered/', signature_name,'.png', sep="")
  
    #Cairo_png(filename=outfile, width=13, height=11)
    #plot(p_value_filtered$score, p_value_filtered$mean, xlab='score',
    #     ylab='Mean connectivity score',main=signature_name)
    #graphics.off()
    print(paste('png written to',outfile))
  
    
  dir.create(paste(outfolder,'downscore_vs_upscore_vs_pvalue/',sep=""), showWarnings = FALSE)
  outfile <- paste(outfolder,'downscore_vs_upscore_vs_pvalue/', signature_name,'.png', sep="")
    #Cairo_png(filename=outfile, width=13, height=11)
    #scatterplot3d(connectivity_data_frame$down, connectivity_data_frame$up, connectivity_data_frame$p,
    #              xlab='Down score',ylab='Up score', zlab='p-value',main=signature_name)
    #graphics.off()
    #print(paste('png written to',outfile))
  
  dir.create(paste(outfolder,'downscore_vs_upscore_vs_pvalue__p_value_filtered/',sep=""), showWarnings = FALSE)
  outfile <- paste(outfolder,'downscore_vs_upscore_vs_pvalue__p_value_filtered/', signature_name,'.png', sep="")
    #Cairo_png(filename=outfile, width=13, height=11)
    #scatterplot3d(p_value_filtered$down, p_value_filtered$up, p_value_filtered$p,
    #              xlab='Down score',ylab='Up score', zlab='p-value',main=signature_name)
    #graphics.off()
    #print(paste('png written to',outfile))
  
  
  dir.create(paste(outfolder,'upscore_minus_downscore_vs_pvalue/',sep=""), showWarnings = FALSE)
  outfile <- paste(outfolder,'upscore_minus_downscore_vs_pvalue/', signature_name,'.png', sep="")
    #Cairo_png(filename=outfile, width=13, height=11)
    #plot(connectivity_data_frame$up - connectivity_data_frame$down, connectivity_data_frame$p,
    #     xlab='Down score - up score',ylab='p-value',main=signature_name)
    #graphics.off()
    #print(paste('png written to',outfile))

  dir.create(paste(outfolder,'upscore_minus_downscore_vs_pvalue__p_value_filtered/',sep=""), showWarnings = FALSE)
  outfile <- paste(outfolder,'upscore_minus_downscore_vs_pvalue__p_value_filtered/', signature_name,'.png', sep="")
    #Cairo_png(filename=outfile, width=13, height=11)
    #plot(p_value_filtered$up - p_value_filtered$down, p_value_filtered$p,
    #     xlab='Down score - up score',ylab='p-value',main=signature_name)
    #graphics.off()
    #print(paste('png written to',outfile))
}

overlap_plot <- function(sets, terms, outfile, xlab, ylab, colnames=FALSE, pch='.', cex=.75, width=15, height=11,
                         mar_lower=4, mar_left=10, main=''){
    #if (colnames==TRUE){xlab=''}
    #Cairo_pdf(filename=outfile, width=width, height=height)
    #mar.default <- c(5,4,4,2) + 0.1
    #par(mar = mar.default + c(mar_lower, mar_left, 0, 0)) 
    #tab <- t(sapply(sets,function(xx)table(factor(xx,levels=terms))))
    #tab[tab>1] <- 1
    #tab <- tab[,names((sort(colSums(tab),decreasing=TRUE)))]
    #plot(c(1,length(terms)), c(1,length(sets)), type="n", xlab=xlab, ylab=ylab, xaxt="n", yaxt="n",main=main)
    #abline(h=seq_along(sets),col="grey",lty=3)  
  
  #  for ( ii in 1:nrow(tab) ) {
  #    points(which(tab[ii,]==1),rep(nrow(tab)-ii+1,sum(tab[ii,])),pch=pch, cex=cex)
  #  }
  #  if (colnames==FALSE){
  #    axis(side=1,at=seq_along(terms),labels=FALSE,las=3)
  #  }
  #  else{
  #    axis(side=1, labels=FALSE)
  #    text(x = seq_along(terms),0, labels = colnames(tab), srt = 45, pos = 2, xpd = TRUE, cex=cex)
    #}
    #axis(side=2,labels=FALSE)
    #text(y = seq_along(sets),par("usr")[1], labels = rev(rownames(tab)), pos = 2, xpd = TRUE, cex=cex)
    #graphics.off()
    #print(paste('plot written to',outfile))
}

genes_samples_dendogram <- function(geneSel, name1, name2, clust_validity, clust_stability, metric, gene_or_sample,
                                    location, png_name){
  #print(paste('Writing dendogram to ',png_name))
  #par(mar=c(9.5,3,0.5, 0.3)) 
  #Cairo_png(filename=png_name, width=13, height=11)
  #if(gene_or_sample=='samples'){
  #  if(metric=='euclidian'){clust <- hclust(dist(t(geneSel)))}
  #  else if(metric=='correlation'){clust<-hclust(dist(((1-cor(geneSel))/2)))}
  #  else{stop(paste(metric,' metric not implemented yet'))}
  #}
  #else if(gene_or_sample=='genes'){
  #  if(metric=='euclidian'){clust <- hclust(dist(geneSel))}
  #  else if(metric=='correlation'){clust<-hclust(dist(((1-cor(t(geneSel)))/2)))}
  #  else{stop(paste(metric,' metric not implemented yet'))}
  #}
  #if (length(clust$labels) > 2){
   # plot(clust, main=paste(name1,"vs",name2))
  #  add_stability_validity_text(clust_validity=clust_validity, clust_stability=clust_stability)
  #  graphics.off()
  #}
  #else{print("Only 2 or less labels found, can't make a dendogram")}
}
