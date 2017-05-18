# on windows, if cairoDevice is giving a problem, see https://stat.ethz.ch/pipermail/r-help/2009-May/199383.html
# and http://gtk-win.sourceforge.net/home/index.php/Main/Downloads
install.packages('cairoDevice',repos = "http://cran.us.r-project.org")
install.packages('clValid',repos = "http://cran.us.r-project.org")
install.packages('VennDiagram',repos = "http://cran.us.r-project.org")
install.packages('gplots',repos = "http://cran.us.r-project.org")
install.packages('ggplot2',repos = "http://cran.us.r-project.org")
install.packages('findpython',repos = "http://cran.us.r-project.org")
install.packages('XLConnect',repos = "http://cran.us.r-project.org")
source("http://bioconductor.org/biocLite.R")
biocLite(c('hgu133a.db', 'GO.db', 'topGO','limma','ArrayExpress','affy','reshape2','Rgraphviz','ALL'))

#install.packages('/Users/NPK/Dropbox/Luxembourg/Data/R_data/drugFinder_no_plots_0.1.tar.gz', repos = NULL, type="source")
install.packages('rJava', type='source',repos = "http://cran.us.r-project.org")
