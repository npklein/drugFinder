write_summary <- function(outfolder_location, zebrafishSel, zebrafishSel_id, name1, name2){
  dir.create(paste(outfolder_location, "summary/", sep=""), showWarnings = FALSE)
  file_name <- paste(outfolder_location,"summary/summary_",name1,"__",name2,".txt",sep="")
  if(!file.exists(file_name)){
    total <- length(rownames(zebrafishSel))
    unknown <- length(grep("X.", rownames(zebrafishSel), fixed=TRUE))
    entrez <- length(zebrafishSel_id)
    fileConn<-file(file_name)
    writeLines(c('total\tno_symbol\tentrez', paste(total, unknown, entrez,sep="\t")), fileConn)
    print(paste('Writing summary to ', file_name))
    close(fileConn)
  }
}


  
