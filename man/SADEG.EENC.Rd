% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ENC.R
\name{SADEG.EENC}
\alias{SADEG.EENC}
\title{SADEG.EENC}
\usage{
SADEG.EENC(Nucleotide_Sequence)
}
\arguments{
\item{Nucleotide_Sequence}{Nucleotide Sequence}
}
\value{
EENC
}
\description{
Expected Effective Number Of Codon
}
\details{
ENC analysis was used to quantify the absolute codon usage bias by determining the degree of codon usage bias exhibited by the coding sequences, regardless of gene length and the number of amino acids. ENC values ranged between 20 and 61. ENC value of 20, indicates extreme codon usage bias using only one of the possible synonymous codons for the corresponding amino acid and value 61 denotes no bias using all possible synonymous codons equally for the corresponding amino acid. A smaller ENC value correlates with the larger extension of codon preference in a gene. It is also generally accepted that when the ENC value is less than or equal to 35, genes tend to have a significant codon bias.
}
\examples{
SADEG.EENC(Nucleotide_Sequence="atggctgctgcagcggccagtcacgatcagaggtaagttgtcgcagcatgt")
}
\author{
Babak Khorsand
}

