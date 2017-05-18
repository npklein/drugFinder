

reference_data <- function(accession){
  ###### mos tof this is taken from http://www.bioconductor.org/packages/release/bioc/vignettes/gCMAPWeb/inst/doc/tutorial.pdf
  # check if it exists, saves a lot of loading time
  accession = "E-GEOD-5258"
  setwd('reference_data')
  accession.files <- getAE(accession, local=TRUE, extract=TRUE)
  setwd('../')

  accession.batch <- ae2bioc(mageFiles=accession.files) 
  # Since its deposition, the first array platform has changed its name, 
  # so we update its annotation string
  annotation(accession.batch[[1]]) <- "hthgu133a"
  
  # As this experiment was performed on two different array platforms, ArrayExpress returns a list
  # with two affyBatch objects, one for each array platform. Normalize each object separately.
  accession.rma <- lapply( accession.batch, rma )
  # map probe IDs to Entrez identifiers and average data for genes assayed by multiple probes.
  accession.eSets <- lapply( accession.rma, mapNmerge)
  
  # Now the two normalized ExpressionSets can be combined into one.
  accession.eSet <- mergeCMAPs( accession.eSets[[1]], accession.eSets[[2]] )
    
  # Next, we identify the experimental factors of interest from the sample annotations provided 
  # by ArrayExpress and shorten them to make them easier to read.
  conditions <- grep("^Factor", varLabels( accession.eSet ), value=TRUE)
  pData( accession.eSet ) <- pData( accession.eSet )[, conditions]
  varLabels( accession.eSet ) <- c("CellLine", "Vehicle","Compound", "Time", "Dose")
  
  # The splitPerturbations function automatically combines matched treatment and control 
  # samples into separate ExpressionSets, one for each tested condition, and returns 
  # them in a list. We are interested in studying the effect of the different
  # Compounds. Controls received treatment "none" and need to be matched to 
  # perturbations performed in the same CellLine, treated with the correct
  # Vehicle and for the same amount of Time.
  accession.list <- splitPerturbations( accession.eSet, factor.of.interest="Compound",control="none", controlled.factors=c("CellLine","Vehicle","Time"))
  sample.anno <- annotate_eset_list( accession.list )
  # We obtain a list with 281 treatment conditions with biological replication, 
  # suitable for differential expression analysis. Again, we use the
  # generate_gCMAP_NChannelSet function to analyze all instances (using limma).
  accession.cmap <- generate_gCMAP_NChannelSet( accession.list, uids=paste( "Exp", 1:length( accession.list ), sep=""), big.matrix=file.path( tempdir(),"accession.cmap"), sample.annotation=sample.anno)
  accession.sets <- induceCMAPCollection( accession.cmap, element="z", higher=3, lower=-3 )
  accession.sets <- minSetSize( accession.sets, min.members=10 )
  return(list(cmap=accession.cmap, sets=accession.sets,pertu_list=accession.list, anno=sample.anno))
  
}