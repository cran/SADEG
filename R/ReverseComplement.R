#' @title SADEG.ReverseComplement
#' @description Complement of the reversed sequence.
#' @details Complement of the reversed sequence. (atcagt -> Reverse: tgacta -> Reverse-Complement: actgat)
#' @author Babak Khorsand
#' @export SADEG.ReverseComplement
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return Reverse-Complement
#' @examples
#' SADEG.ReverseComplement("actagtcacgatcag")
SADEG.ReverseComplement = function(Nucleotide_Sequence)
{
  ReverseComplement=NULL
  Nucleotide_Sequence=toupper(Nucleotide_Sequence)
  Nucleotide_Sequence=unlist(strsplit(Nucleotide_Sequence,""))
  Nucleotide_Sequence=Nucleotide_Sequence[length(Nucleotide_Sequence):1]
  a=grep("A",Nucleotide_Sequence)
  t=grep("T",Nucleotide_Sequence)
  c=grep("C",Nucleotide_Sequence)
  g=grep("G",Nucleotide_Sequence)
  Nucleotide_Sequence[a]="T"
  Nucleotide_Sequence[t]="A"
  Nucleotide_Sequence[c]="G"
  Nucleotide_Sequence[g]="C"
  ReverseComplement=paste(Nucleotide_Sequence,collapse = "")
  names(ReverseComplement)="ReverseComplement"
  return(ReverseComplement)
}
