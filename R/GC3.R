#' @title SADEG.GC3
#' @description GC3 content.
#' @details GC3 content : Sum of frequencies of G and C at the third position of each codon.
#' @author Babak Khorsand
#' @export SADEG.GC3
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return GC3
#' @examples
#' SADEG.GC3(Nucleotide_Sequence="atggctgctgcagcggccagtcacgatcagaggtaagttgtcgcagcatgt")
SADEG.GC3 = function(Nucleotide_Sequence)
{
  RSCU_List=NULL
  Nucleotide_Sequence=toupper(Nucleotide_Sequence)
  Codon=sapply(0:((nchar(Nucleotide_Sequence)/3)-1), function(x) substr(Nucleotide_Sequence,(x*3)+1,(x*3)+3))
  Codon_Table=table(Codon)
  Spare=c("ATG","TGG","TAG","TAA","TGA")
  Codons=Codon_Table[-which(names(Codon_Table) %in% Spare)]
  L=sum(Codons)
  GC3=sum(Codons[which(substr(names(Codons),3,3) %in% c("C","G"))])
  names(GC3)="GC3"
  if (GC3>0)
    return(GC3/L)
}
