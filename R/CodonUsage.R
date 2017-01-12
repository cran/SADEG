#' @title SADEG.CodonUsage
#' @description Frequency of occurrence of each Amino acids codons.
#' @details Frequency of occurrence of each Amino acids codons.
#' @author Babak Khorsand
#' @export SADEG.CodonUsage
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return CodonUsage
#' @examples
#' SADEG.CodonUsage(Nucleotide_Sequence="atggctgcagcggccagtcacgatcagaggtaagttgtc")
SADEG.CodonUsage = function(Nucleotide_Sequence)
{
  CodonUsage_List=NULL
  Nucleotide_Sequence=toupper(Nucleotide_Sequence)
  Codon=sapply(0:((nchar(Nucleotide_Sequence)/3)-1), function(x) substr(Nucleotide_Sequence,(x*3)+1,(x*3)+3))
  Codon_Table=table(Codon)
  codon_List=strsplit(codon_List,",")
  CodonUsage_List=sapply(codon_List, function(x) sapply(x[-1], function(y) ifelse(is.na(Codon_Table[y]),0,Codon_Table[y])))
  for (i in 1:21)
  {
    names(CodonUsage_List[[i]])= codon_List[[i]][-1]
  }
  names(CodonUsage_List)=sapply(codon_List, function(x) x[1])
  CodonUsage_List=sapply(CodonUsage_List, function(x) {sum=sum(x); sapply(x, function(y) ifelse(sum==0,0,y/sum))})
  CodonUsage_List=unlist(CodonUsage_List)
  names(CodonUsage_List)=gsub(".*\\.(...)","\\1",names(CodonUsage_List))
  return(CodonUsage_List)
}
