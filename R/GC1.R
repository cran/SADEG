#' @title SADEG.GC1
#' @description GC1 content.
#' @details GC1 content : Sum of frequencies of G and C at the first position of each codon.
#' @author Babak Khorsand
#' @export SADEG.GC1
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return GC1
#' @examples
#' SADEG.GC1(Nucleotide_Sequence="atggctgctgcagcggccagtcacgatcagaggtaagttgtcgcagcatgt")
SADEG.GC1 = function(Nucleotide_Sequence)
{
  RSCU_List=NULL
  Nucleotide_Sequence=toupper(Nucleotide_Sequence)
  Codon=sapply(0:((nchar(Nucleotide_Sequence)/3)-1), function(x) substr(Nucleotide_Sequence,(x*3)+1,(x*3)+3))
  Codon_Table=table(Codon)
  Spare=c("ATG","TGG","TAG","TAA","TGA")
  Codons=Codon_Table[-which(names(Codon_Table) %in% Spare)]
  L=sum(Codons)
  GC1=sum(Codons[which(substr(names(Codons),1,1) %in% c("C","G"))])
  names(GC1)="GC1"
  if (GC1>0)
    return(GC1/L)
}
