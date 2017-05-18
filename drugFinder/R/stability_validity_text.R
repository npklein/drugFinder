
#' <todo>
#' 
#' <todo>
#' 
add_stability_validity_text <- function(clust_validity, clust_stability){
  # Connectivity doesn't work for some reason
  #mtext("Samples", line = 3, adj = 1)
  mtext(paste("Dunn: ",measures(clust_validity$samples,"Dunn")), line = 2, adj = 1)
  #mtext(paste("Connectivity: ",measures(clust_validity_samples,"Connectivity")), line = 1, adj = 1)
  mtext(paste("Silhouette: ",measures(clust_validity$samples,"Silhouette")), line = 1, adj = 1)
  mtext(paste("APN: ",measures(clust_stability$samples,"APN")), line = 0, adj = 1)
  mtext(paste("AD: ",measures(clust_stability$samples,"AD")), line = -1, adj = 1)
  mtext(paste("ADM: ",measures(clust_stability$samples,"ADM")), line = -2, adj = 1)
  #mtext("Genes", line = -4, adj = 1)
  #mtext(paste("Dunn: ",measures(clust_validity$genes,"Dunn")), line = -5, adj = 1)
  #mtext(paste("Connectivity: ",measures(clust_validity_genes,"Connectivity")), line = -4, adj = 1)
  #mtext(paste("Silhouette: ",measures(clust_validity$genes,"Silhouette")), line = -6, adj = 1)
  #mtext(paste("APN: ",measures(clust_stability$genes,"APN")), line = -7, adj = 1)
  #mtext(paste("AD: ",measures(clust_stability$genes,"AD")), line = -8, adj = 1)
  #mtext(paste("ADM: ",measures(clust_stability$genes,"ADM")), line = -9, adj = 1)
}