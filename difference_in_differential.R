

# loading the data
if(!(exists("zebrafish_expression_full"))){ 
  zebrafish_expression_full = read.table("K:\\GENO\ Public\\Petr\\INFUSED\\2013-12-20\\DataCE.txt", sep='\t', header=TRUE, quote="")
}
rownames(zebrafish_expression_full) <- make.names(zebrafish_expression_full[,'Gene.Symbol'], unique=TRUE)
sham1 <- c('Sham1.r1', 'Sham1.r2', 'Sham1.r3')
sham2 <- c('Sham2.r1', 'Sham2.r2', 'Sham2.r3') 
sham3 <- c('Sham3.r1', 'Sham3.r2', 'Sham3.r3')
shams <- list(sham1, sham2, sham3)
hour4 <- c('T00d4h.r2', 'T00d4h.r3', 'T00d4h.r4')
day1  <- c('T01d.r1', 'T01d.r2', 'T01d.r4')
day3  <- c('T03d.r1', 'T03d.r2', 'T03d.r3')
day7  <- c('T07d.r1', 'T07d.r2', 'T07d.r3')
day14 <- c('T14d.r1', 'T14d.r2', 'T14d.r3')
day90 <- c('T90d.r1', 'T90d.r2', 'T90d.r3')
measurements <- list(hour4, day1, day3, day7, day14, day90)

s <- seq(length(all))
uniq.pairs <- unique(as.data.frame(t(apply(expand.grid(s, s), 1, sort))))
combinations <- apply(uniq.pairs, 1, function(x) all[x])

sham1_differential <- list()
sham2_differential <- list()
sham3_differential <- list()

# R version of a dictionary
days <- vector(mode="list", length=6)
names(days) <- c("T00d4h", "T01d", "T03d", "T07d", "T14d", "T90d")
days[[1]] <- 1; days[[2]] <- 2; days[[3]] <- 3; days[[4]] <- 4; days[[5]] <- 5; days[[6]] <- 6

for(combination in combinations){
  combination <- unlist(combination)
  # we don't want to compare two of the same to each other, so continue
  if(combination[1]==combination[4]){next}
  zebrafish_expression  <- zebrafish_expression_full[,combination]
  zebrafish_matrix <- data.matrix(zebrafish_expression)
  f <- factor(as.character(c(
    "FIRST","FIRST","FIRST",
    "SECOND","SECOND","SECOND")))
  design <- model.matrix(~f)
  fit <- eBayes(lmFit(zebrafish_matrix, design))
  gene_list <- topTable(fit, coef=2, number=1000000, sort.by="logFC")

  ##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off
  no_of_genes = dim(fit)[1]
  gene_list$threshold = as.factor(abs(gene_list$logFC) > 2 & gene_list$P.Value < 0.05/no_of_genes)
  
  name1 = unlist(strsplit(combination[1],".",fixed=TRUE))[1]
  name2 = unlist(strsplit(combination[4],".",fixed=TRUE))[1]
  if(name1=='Sham1'){
    sham1_differential[unlist(days[name2])] <- length(which(gene_list[,7]==TRUE))
  }
  else if (name2=='Sham1'){
    sham1_differential[unlist(days[name1])] <- length(which(gene_list[,7]==TRUE))
  }
  if(name1=='Sham2'){
    sham2_differential[unlist(days[name2])] <- length(which(gene_list[,7]==TRUE))
  }
  else if (name2=='Sham2'){
    sham2_differential[unlist(days[name1])] <- length(which(gene_list[,7]==TRUE))
  }
  if(name1=='Sham3'){
    sham3_differential[unlist(days[name2])] <- length(which(gene_list[,7]==TRUE))
  }
  else if (name2=='Sham3'){
    sham3_differential[unlist(days[name1])] <- length(which(gene_list[,7]==TRUE))
  }
}


require(reshape)
measurements <- c('4hpi', '1dpi', '3dpi', '7dpi','14dpi','90dpi')
df <- data.frame(measurements=measurements, sham1=unlist(sham1_differential), sham2=unlist(sham2_differential), control=unlist(sham3_differential))
df <- melt(df, id='measurements', variable_name = 'shams')
df <- transform(df, measurements = factor(measurements, level = measurements))
ggplot(df, aes(measurements, value)) + geom_line(aes(colour = shams, group=shams)) + 
                                      ylab("# of differential 2fold change genes")
png_name = "C:/Users/ndeklein/Desktop/Figures/Automatic/differential_comparison.png"
ggsave(filename=png_name)
graphics.off()
