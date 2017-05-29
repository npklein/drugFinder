install.packages('drugFinder.tar.gz',repo=NULL)
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
library(drugFinder)


#### Step 1: Read in data. In this case we will use "airway" data from bioconductor
load('INFUSED_microarray_data.RData')

#### Step 2: Define first signature signatures

# set variable for technical/biological replicates
sham3 <- c('Sham3.r1', 'Sham3.r2', 'Sham3.r3')
hour4 <- c('T00d4h.r2', 'T00d4h.r3', 'T00d4h.r4')
day1  <- c('T01d.r1', 'T01d.r2', 'T01d.r4')
day3  <- c('T03d.r1', 'T03d.r2', 'T03d.r3')
day7  <- c('T07d.r1', 'T07d.r2', 'T07d.r3')
day14 <- c('T14d.r1', 'T14d.r2', 'T14d.r3')

# select which columns to use (these column names have to exist in the input data from Step 1)
columns <- c(day1, day3, sham3)

# define which columns belong to the same group (e.g. day1 and day3 vs sham3) by 
# setting FIRST and SECOND.
f <- factor(as.character(c(
  'SECOND','SECOND','SECOND',
  'SECOND','SECOND','SECOND',
  'FIRST','FIRST','FIRST')))

# add the data together to define the signature. The combination of these will be used for output names
# of files
day1_day3__sham3 <- list(columns=columns, f=f, name1='day1_day3', name2='sham3')


#### Step 3: Do the same thing for other signatures you might want to use

# second signature
columns <- c(day7, day14,sham3)
f <- factor(as.character(c(
  'SECOND','SECOND','SECOND',
  'SECOND','SECOND','SECOND',
  'FIRST','FIRST','FIRST')))
day7_day14__sham3 <- list(columns=columns, f=f, name1='day7_day14', name2='sham3')

# third signature
columns <- c(day1, day3, hour4)
f <- factor(as.character(c(
  'SECOND','SECOND','SECOND',
  'SECOND','SECOND','SECOND',
  'FIRST','FIRST','FIRST')))
day1_day3__hour4 <- list(columns=columns, f=f, name1='day1_day3', name2='hour4')

#### Step 4: Combine the signatures into groups that belong together. 
####         E.g. I want to search with signature 1 + signature 2 and
####              with signature 3
datasets <- list('signature_group_1'=list(day1_day3__sham3,day7_day14__sham3),
                 'signature_group_2'=list(day1_day3__hour4
                 ))

#### Step 4: Search for drugs
find_drugs(INFUSED_microarray_data, datasets, '/tmp/')




