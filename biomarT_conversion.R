symbol_to_entrez <- function(symbols){
  ensembl = useMart("ensembl",dataset="hsapiens_gene_ensembl")
  #building a query, requires filters, attributes and values
  #listFilters shows all filters
  return(getBM(attributes=c('hgnc_symbol', 'entrezgene'), filters = 'hgnc_symbol', values = symbols, mart = ensembl))
}
