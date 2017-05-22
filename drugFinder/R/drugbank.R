
get_drugbank_info <- function(drugnames_file,outfile){
  python_location <- find_python_cmd(minimum_version='3.0',required_modules=c('requests','re','html2text','argparse','http.cookiejar','urllib','bs4'))
  py_loc = paste(python_location, 'drugbank.py')
  if(!file.exists(outfile)){
     command <- paste(py_loc,'-i',drugnames_file,'-o',outfile)
     print(command)
     system(command)
     print(paste('Writing drugbank codes to',outfile))
  }
  drugcodes <- read.table(outfile,sep='\t',header = TRUE)
  rownames(drugcodes) <- drugcodes$drug_name  
  return(drugcodes)
}
