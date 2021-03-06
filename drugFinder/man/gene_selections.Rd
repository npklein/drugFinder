% Generated by roxygen2 (4.0.1): do not edit by hand
\name{gene_selections}
\alias{gene_selections}
\title{Select genes for the signature}
\usage{
gene_selections(microarray_data, input_data, strict = TRUE,
  half_strict = FALSE, relaxed = FALSE)
}
\arguments{
\item{microarray_data}{The microarray input data}

\item{input_data}{The input_data has a variable f that defines what the control and what the desired state is}

\item{strict}{If the output should contain the strictly selected genes. This is 2 log2 fold change and p-value < 0.05 after Bonferronni correction (default: TRUE)}

\item{half_strict}{If the output should contain the half strictly selected genes. This is 1.5 log2 fold change and p-value < 0.05 after Bonferronni correction (default: FALSE)}

\item{half_strict}{If the output should contain the relaxed selected genes. This is 1.5 log2 fold change and p-value < 0.05 after Holms correction (default: FALSE)}
}
\value{
Differential genes (from the signature)
}
\description{
Takes the microarray data and identifies the differential genes to use as signature genes.
}

