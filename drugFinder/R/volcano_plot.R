
volcano_plot <- function(fit, name1, name2, location, png_name){
  par(mar=c(9.5,3,0.5, 0.3)) 
  gene_list <- topTable(fit, coef=2, number=1000000, sort.by="logFC")
  ##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off
  no_of_genes = dim(fit)[1]
  gene_list$threshold = as.factor(abs(gene_list$logFC) > 2 & gene_list$P.Value < 0.05/no_of_genes)
  ##Construct the plot object
  #ggplot(data=gene_list, aes(x=logFC, y=-log10(P.Value), colour=threshold)) +  
  #  geom_point(alpha=0.4, size=1.75) +  theme(legend.position = "none") +  
  #  xlim(c(-10, 10)) + ylim(c(0, 15)) + xlab("log2 fold change") + ylab("-log10 p-value") + 
  #  ggtitle(paste(name1,"vs",name2))
  #print(paste('Writing volcano plot to ',png_name))
  #ggsave(filename=png_name, width=10)
  #graphics.off()
}