#' @title SADEG.GC12
#' @description GC12 content.
#' @details GC12 content : Sum of frequencies of G and C at first and second position of each codon.
#' @author Babak Khorsand
#' @export SADEG.GC12
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return GC12
#' @examples
#' SADEG.GC12(Nucleotide_Sequence="atggctgctgcagcggccagtcacgatcagaggtaagttgtcgcagcatgt")
SADEG.GC12 = function(Nucleotide_Sequence)
{

  RSCU_List=NULL
  Nucleotide_Sequence=toupper(Nucleotide_Sequence)
  Codon=sapply(0:((nchar(Nucleotide_Sequence)/3)-1), function(x) substr(Nucleotide_Sequence,(x*3)+1,(x*3)+3))
  Codon_Table=table(Codon)
  Spare=c("ATG","TGG","TAG","TAA","TGA")
  Codons=Codon_Table[-which(names(Codon_Table) %in% Spare)]
  L=sum(Codons)
  GC1=sum(Codons[which(substr(names(Codons),1,1) %in% c("C","G"))])/L
  GC2=sum(Codons[which(substr(names(Codons),2,2) %in% c("C","G"))])/L
  GC12=(GC1+GC2)/2
  names(GC12)="GC12"
  if (GC2>0)
    return(GC12)
}
