#' @title SADEG.RSCU
#' @description Relative Synonymous Codon Usage
#' @details The Relative Synonymous Codon Usage (RSCU) is the number of times a codon appears in a gene divided by the number of expected occurrences under equal codon usage.
#' @author Babak Khorsand
#' @export SADEG.RSCU
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return RSCU
#' @examples
#' SADEG.RSCU(Nucleotide_Sequence="atggctgcagcggccagtcacgatcagaggtaagttgtc")
SADEG.RSCU = function(Nucleotide_Sequence)
{
  RSCU_List=NULL
  Nucleotide_Sequence=toupper(Nucleotide_Sequence)
  Codon=sapply(0:((nchar(Nucleotide_Sequence)/3)-1), function(x) substr(Nucleotide_Sequence,(x*3)+1,(x*3)+3))
  Codon_Table=table(Codon)
  codon_List=strsplit(codon_List,",")
  RSCU_List=sapply(codon_List, function(x) sapply(x[-1], function(y) ifelse(is.na(Codon_Table[y]),0,Codon_Table[y])))
  for (i in 1:21)
  {
    names(RSCU_List[[i]])= codon_List[[i]][-1]
  }
  names(RSCU_List)=sapply(codon_List, function(x) x[1])
  RSCU_List=sapply(RSCU_List, function(x) {mean_x=mean(x); sapply(x, function(y) ifelse(mean_x==0,0,y/mean_x))})
  RSCU_List=unlist(RSCU_List)
  names(RSCU_List)=gsub(".*\\.(...)","\\1",names(RSCU_List))
  return(RSCU_List)
}

