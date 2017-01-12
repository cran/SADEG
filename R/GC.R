#' @title SADEG.GC
#' @description GC content.
#' @details GC content : Sum of frequencies of G and C.
#' @author Babak Khorsand
#' @export SADEG.GC
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return GC content
#' @examples
#' SADEG.GC("actagtcacgatcag")
SADEG.GC = function(Nucleotide_Sequence)
{
  GC=NULL
  Nucleotide_Sequence=tolower(Nucleotide_Sequence)
  GC=round((table(strsplit(Nucleotide_Sequence,""))["g"]+table(strsplit(Nucleotide_Sequence,""))["c"])/nchar(Nucleotide_Sequence),3)
  names(GC)="GC"
  return(GC)
}
