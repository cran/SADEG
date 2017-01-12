#' @title SADEG.GC2
#' @description GC2 content.
#' @details GC2 content : Sum of frequencies of G and C at the second position of each codon.
#' @author Babak Khorsand
#' @export SADEG.GC2
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return GC2
#' @examples
#' SADEG.GC2(Nucleotide_Sequence="atggctgctgcagcggccagtcacgatcagaggtaagttgtcgcagcatgt")
SADEG.GC2 = function(Nucleotide_Sequence)
{
  RSCU_List=NULL
  Nucleotide_Sequence=toupper(Nucleotide_Sequence)
  Codon=sapply(0:((nchar(Nucleotide_Sequence)/3)-1), function(x) substr(Nucleotide_Sequence,(x*3)+1,(x*3)+3))
  Codon_Table=table(Codon)
  Spare=c("ATG","TGG","TAG","TAA","TGA")
  Codons=Codon_Table[-which(names(Codon_Table) %in% Spare)]
  L=sum(Codons)
  GC2=sum(Codons[which(substr(names(Codons),2,2) %in% c("C","G"))])
  names(GC2)="GC2"
  if (GC2>0)
    return(GC2/L)
}
