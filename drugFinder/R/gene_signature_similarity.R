


spearman_similarity <- function(signature1, signature2){
  signature_overlap <- intersect(rownames(signature2$gene_list), rownames(signature1$gene_list))
  signature2_gene_list <- signature2$gene_list[signature_overlap,]
  signature2_gene_list <- signature2_gene_list[apply(!is.na(signature2_gene_list), 1, any), ]
  signature1_gene_list <- signature1$gene_list[signature_overlap,]
  signature1_gene_list <- signature1_gene_list[apply(!is.na(signature1_gene_list), 1, any), ]
  print(signature1_gene_list)
  print(signature2_gene_list)
  return(cor(signature1_gene_list$logFC,signature2_gene_list$logFC,method='spearman'))
}

pearson_similarity <- function(signature1, signature2){
  signature_overlap <- intersect(rownames(signature2$gene_list), rownames(signature1$gene_list))
  signature2_gene_list <- signature2$gene_list[signature_overlap,]
  signature2_gene_list <- signature2_gene_list[apply(!is.na(signature2_gene_list), 1, any), ]
  signature1_gene_list <- signature1$gene_list[signature_overlap,]
  signature1_gene_list <- signature1_gene_list[apply(!is.na(signature1_gene_list), 1, any), ]
  return(cor(signature1_gene_list$logFC,signature2_gene_list$logFC,method='pearson'))
}

kendall_similarity <- function(signature1, signature2){
  signature_overlap <- intersect(rownames(signature2$gene_list), rownames(signature1$gene_list))
  signature2_gene_list <- signature2$gene_list[signature_overlap,]
  signature2_gene_list <- signature2_gene_list[apply(!is.na(signature2_gene_list), 1, any), ]
  signature1_gene_list <- signature1$gene_list[signature_overlap,]
  signature1_gene_list <- signature1_gene_list[apply(!is.na(signature1_gene_list), 1, any), ]
  return(cor(signature1_gene_list$logFC,signature2_gene_list$logFC,method='kendall'))
}

GSEA_similarity <- function(signature1, signature2){
  warning('Nothing writting for "GSEA_similarity" yet')
}

Kolomogorov_Smirnov_based_statistic_similarity <- function(signature1, signature2){
  warning('Nothing writting for "Kolomogorov_Smirnov_based_statistic_similarity" yet')
}
