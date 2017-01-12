#' @title SADEG.ACGT
#' @description Number and frequencies of occurrence of each nucleotide.
#' @details The Number of occurrence of each nucleotide.(A, T, C, and G). Furtheremore, The frequencies of occurrence of each nucleotide. (A %, T %, C %, and G %).
#' @author Babak Khorsand
#' @export SADEG.ACGT
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return Table which first row represent number of each nucleotide and row 2 represent percentage of each nucleotide.
#' @examples
#' SADEG.ACGT("actagtcacgatcag")
SADEG.ACGT = function(Nucleotide_Sequence)
{
  ACGT=NULL
  Nucleotide_Sequence=tolower(Nucleotide_Sequence)
  ACGT=table(strsplit(Nucleotide_Sequence,""))
  ACGT=data.frame(a=ACGT[1],c=ACGT[2],g=ACGT[3],t=ACGT[4])
  ACGT=rbind(ACGT,c(ACGT[1,1]/sum(ACGT[1,]),ACGT[1,2]/sum(ACGT[1,]),ACGT[1,3]/sum(ACGT[1,]),ACGT[1,4]/sum(ACGT[1,])))
  ACGT=t(apply(ACGT,1,function(x) round(x,digits = 2)))
  ACGT[1,]=round(ACGT[1,])
  rownames(ACGT)=c("Number","Percent")
  return(ACGT)
}

