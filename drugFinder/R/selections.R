#' Select genes for the signature
#' 
#' Takes the microarray data and identifies the differential genes to use as signature genes.
#' @param microarray_data The microarray input data
#' @param input_data The input_data has a variable f that defines what the control and what the desired state is
#' @param strict If the output should contain the strictly selected genes. This is 2 log2 fold change and p-value < 0.05 after Bonferronni correction (default: TRUE)
#' @param half_strict If the output should contain the half strictly selected genes. This is 1.5 log2 fold change and p-value < 0.05 after Bonferronni correction (default: FALSE)
#' @param half_strict If the output should contain the relaxed selected genes. This is 1.5 log2 fold change and p-value < 0.05 after Holms correction (default: FALSE)
#' @return Differential genes (from the signature)
#' 
gene_selections <- function(microarray_data, input_data, strict=TRUE, half_strict=FALSE, relaxed=FALSE){
  zebrafish_expression  <- microarray_data[,input_data$columns]
  zebrafish_matrix <- data.matrix(zebrafish_expression)
  f = input_data$f
  design <- model.matrix(~f)
  fit <- eBayes(lmFit(zebrafish_matrix, design))
  selected  <- p.adjust(fit$p.value[, 2], "holm") <0.05
  
  gene_list <- topTable(fit, coef=2, number=1000000, sort.by="logFC")
  ##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off
  no_of_genes = dim(fit)[1]
  gene_list$threshold = as.factor(abs(gene_list$logFC) > 2 & gene_list$P.Value < 0.05/no_of_genes)
  selected_strict = rownames(gene_list[gene_list$threshold==TRUE,])
  gene_list$threshold = as.factor(abs(gene_list$logFC) > 1.5 & gene_list$P.Value < 0.05/no_of_genes)
  selected_15fold_strict = rownames(gene_list[gene_list$threshold==TRUE,])
  zebrafishSel <- zebrafish_matrix [selected, ]
  zebrafishSel_strict <- zebrafish_matrix [selected_strict, ]
  zebrafishSel_slightly_strict <- zebrafish_matrix [selected_15fold_strict, ]
  genes <- list()
  if(strict == TRUE){
    genes$strict <- zebrafishSel_strict  
  }
  if(half_strict == TRUE){
    genes$half_strict <- zebrafishSel_slightly_strict
  }
  if(relaxed == TRUE){
    genes$relaxed <- zebrafishSel
  }
   return(list(genes=genes, f=f, 
              fit=fit,gene_list=gene_list))
}


  
