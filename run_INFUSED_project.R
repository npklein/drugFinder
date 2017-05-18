#' @examples ############ Searching with two different signature groups ############
#' # To make the notation shorter later, this example makes a vector containing the names of the technical replicate of each sample
#' sham3 <- c('Sham3.r1', 'Sham3.r2', 'Sham3.r3')
#' hour4 <- c('T00d4h.r2', 'T00d4h.r3', 'T00d4h.r4')
#' day1  <- c('T01d.r1', 'T01d.r2', 'T01d.r4')
#' day3  <- c('T03d.r1', 'T03d.r2', 'T03d.r3')
#' day7  <- c('T07d.r1', 'T07d.r2', 'T07d.r3')
#' day14 <- c('T14d.r1', 'T14d.r2', 'T14d.r3')
#' # SIGNATURE 1
#' columns <- c(day1, day3, sham3)
#' f <- factor(as.character(c(
#'  'SECOND','SECOND','SECOND',
#'  'SECOND','SECOND','SECOND',
#'  'FIRST','FIRST','FIRST')))
#' day1_day3__sham3 <- list(columns=columns, f=f, name1='day1_day3', name2='sham3')
#' # SIGNATURE 2
#' columns <- c(day7, day14,sham3)
#' f <- factor(as.character(c(
#'  'SECOND','SECOND','SECOND',
#'  'SECOND','SECOND','SECOND',
#'  'FIRST','FIRST','FIRST')))
#' day7_day14__sham3 <- list(columns=columns, f=f, name1='day7_day14', name2='sham3')
#' # SIGNATURE 3
#' columns <- c(day1, day3, hour4)
#' f <- factor(as.character(c(
#'  'SECOND','SECOND','SECOND',
#'  'SECOND','SECOND','SECOND',
#'  'FIRST','FIRST','FIRST')))
#' day1_day3__hour4 <- list(columns=columns, f=f, name1='day1_day3', name2='hour4')
#' # datasets with 2 signature groups, each group contains 2 signatures
#' datasets <- list('example'=list('signature_group_1'=list(day1_day3__sham3,
#'                                           day7_day14__sham3),
#'                  'signature_group_2'=list(day1_day3__hour4, 
#'                                           day7_day14__sham3)))                
#' load(system.file('data', 'INFUSED_microarray_data.Rdata', package='drugFinder'))
#' find_drugs(microarray_data, datasets, 'C:/Users/ndeklein/Desktop/Figures/Automatic')
#' #######################################################################    




#detach("package:drugFinder", unload=TRUE)
library(drugFinder)
sham3 <- c('Sham3.r1', 'Sham3.r2', 'Sham3.r3')
hour4 <- c('T00d4h.r2', 'T00d4h.r3', 'T00d4h.r4')
day1  <- c('T01d.r1', 'T01d.r2', 'T01d.r4')
day3  <- c('T03d.r1', 'T03d.r2', 'T03d.r3')
day7  <- c('T07d.r1', 'T07d.r2', 'T07d.r3')
day14 <- c('T14d.r1', 'T14d.r2', 'T14d.r3')
columns <- c(day1, day3, sham3)
f <- factor(as.character(c(
  'SECOND','SECOND','SECOND',
  'SECOND','SECOND','SECOND',
  'FIRST','FIRST','FIRST')))
day1_day3__sham3 <- list(columns=columns, f=f, name1='day1_day3', name2='sham3')
columns <- c(day7, day14,sham3)
f <- factor(as.character(c(
  'SECOND','SECOND','SECOND',
  'SECOND','SECOND','SECOND',
  'FIRST','FIRST','FIRST')))
day7_day14__sham3 <- list(columns=columns, f=f, name1='day7_day14', name2='sham3')
columns <- c(day1, day3, hour4)
f <- factor(as.character(c(
  'SECOND','SECOND','SECOND',
  'SECOND','SECOND','SECOND',
  'FIRST','FIRST','FIRST')))
day1_day3__hour4 <- list(columns=columns, f=f, name1='day1_day3', name2='hour4')
datasets <- list('signature_group_1'=list(day1_day3__sham3,day7_day14__sham3),
                  'signature_group_2'=list(day1_day3__hour4
                                           ))
load("../data/INFUSED_microarray_data.Rdata")
find_drugs(INFUSED_microarray_data, infused_datalist, 'new_results/')
