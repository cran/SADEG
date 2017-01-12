#' @title SADEG.EENC
#' @description Expected Effective Number Of Codon
#' @details ENC analysis was used to quantify the absolute codon usage bias by determining the degree of codon usage bias exhibited by the coding sequences, regardless of gene length and the number of amino acids. ENC values ranged between 20 and 61. ENC value of 20, indicates extreme codon usage bias using only one of the possible synonymous codons for the corresponding amino acid and value 61 denotes no bias using all possible synonymous codons equally for the corresponding amino acid. A smaller ENC value correlates with the larger extension of codon preference in a gene. It is also generally accepted that when the ENC value is less than or equal to 35, genes tend to have a significant codon bias.
#' @author Babak Khorsand
#' @export SADEG.EENC
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return EENC
#' @examples
#' SADEG.EENC(Nucleotide_Sequence="atggctgctgcagcggccagtcacgatcagaggtaagttgtcgcagcatgt")
SADEG.EENC = function(Nucleotide_Sequence)
{
  RSCU_List=NULL
  Nucleotide_Sequence=toupper(Nucleotide_Sequence)
  Codon=sapply(0:((nchar(Nucleotide_Sequence)/3)-1), function(x) substr(Nucleotide_Sequence,(x*3)+1,(x*3)+3))
  Codon_Table=table(Codon)
  Spare=c("ATG","TGG","TAG","TAA","TGA")
  Codons=Codon_Table[-which(names(Codon_Table) %in% Spare)]
  L=sum(Codons)
  S=sum(Codons[which(substr(names(Codons),3,3) %in% c("C","G"))])
  S=S/L
  EENC=2+S+29/(S^2+(1-S)^2)
  names(EENC)="EENC"
  if (EENC>0)
    return(EENC)
}
