
get_atc_info <- function(atc_codes_file,outfile_full_name,outfile_description){
  python_location <- find_python_cmd(minimum_version='3.0',required_modules=c('requests','re','html2text','argparse','http.cookiejar','urllib','bs4'))
  py_loc = paste(python_location, system.file('exec', 'atc_info.py', package='drugFinder'))
  py_loc = 'python3 /Users/NPK/Dropbox/Luxembourg/Data/R_data/drugFinder_no_plots/exec/atc_info.py'
  if((!file.exists(outfile_full_name) || !file.exists(outfile_description))){
     command <- paste(py_loc,'-a',atc_codes_file,'-o',outfile_full_name,'-d',outfile_description)
     print(command)
     system(command)
     print(paste('Writing full names to',outfile_full_name))
     print(paste('Writing descriptions to to',outfile_description))
  }
  atc_info <- list('full_name' = read.table(outfile_full_name,sep='\t',header = TRUE),
                   'info' = read.table(outfile_description,sep='\t',header=TRUE))
  return(atc_info)
}