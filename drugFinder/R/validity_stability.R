#' Calculate the validity of the clustering of samples
#' 
#' For each signature a heatmap, among other results, is created. In the heatmap the samples are clustered
#' according to their expression profiles. This calculates the validity of that clustering
#' @param geneSel The selected genes
#' @param metric The metric to use for clustering (see clValid documentation for all the options)
#' 
validity <- function(geneSel, metric){
  # number of clusters set to 2, as there are always 2 comparisons
  genes_clust <- hclust(dist(geneSel))
  if (length(genes_clust$labels) > 2){
    clust_cutoff <- length(unique(cutree(genes_clust, h = 4)))
    # hacky, doesn't work with 1 clust or more than 5 (might be problematic)
    if (clust_cutoff == 1){clust_cutoff <- 2}
    if (clust_cutoff > 5){clust_cutoff <- 5}
    clust_validity_genes <- clValid(geneSel, clust_cutoff, clMethods=c("agnes"), 
                                    validation="internal", method="complete", maxitems=10000, 
                                    metric=metric)
  }
  else{print("Only 2 or less labels found, can't make use agnes")}
  clust_validity_samples <- clValid(t(geneSel), 2, clMethods=c("agnes"), 
                                    validation="internal", method="complete", 
                                    maxitems=10000, metric=metric)
  clust_validity <- list("samples" = clust_validity_samples, "genes" = clust_validity_genes)
  return(clust_validity)
}

#' Calculate the stability of the clustering of samples
#' 
#' For each signature a heatmap, among other results, is created. In the heatmap the samples are clustered
#' according to their expression profiles. This calculates the stability of that clustering
#' @param geneSel The selected genes
#' @param metric The metric to use for clustering (see clValid documentation for all the options)
#' 
stability <- function(geneSel, metric){
  genes_clust <- hclust(dist(geneSel))
  if (length(genes_clust$labels) > 2){
    clust_cutoff <- length(unique(cutree(genes_clust, h = 4)))
    # hacky, doesn't work with 1 clust or more than 5 (might be problematic)
    if (clust_cutoff == 1){clust_cutoff <- 2}
    if (clust_cutoff > 5){clust_cutoff <- 5}
    clust_stability_genes <- clValid(geneSel, clust_cutoff, clMethods=c("agnes"),
                                     validation="stability", maxitems=10000,
                                     metric=metric)
  
  clust_stability_samples <- clValid(t(geneSel), 2, clMethods=c("agnes"),
                                     validation="stability", maxitems=10000,
                                     metric=metric)
  }
  clust_stability <- list("samples" = clust_stability_samples, "genes" = clust_stability_genes)
  return(clust_stability)
}