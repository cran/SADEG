#' @title SADEG.Dimer
#' @description Number and frequency of each Dimers
#' @details Count the number of each Dimers and calculate frequencies of them.
#' @author Babak Khorsand
#' @export SADEG.Dimer
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return Table which first row represent number of each Dimers and row 2 represent percentage of each Dimers.
#' @examples
#' SADEG.Dimer("actagtcacgatcag")
SADEG.Dimer = function(Nucleotide_Sequence)
{
  TwoMer=NULL
  Nucleotide_Sequence=tolower(Nucleotide_Sequence)
  Nucleotide_Sequence=strsplit(Nucleotide_Sequence,"")
  Nucleotide_Sequence=Nucleotide_Sequence[[1]]
  Nuc_Length=length(Nucleotide_Sequence)
  Twomer_Seq="AA"
  Seq_Num=1:(Nuc_Length-1)
  Twomer_Seq=sapply(Seq_Num, function(i) c(Twomer_Seq,paste(Nucleotide_Sequence[i],Nucleotide_Sequence[i+1],sep="")))
  Twomer_Seq=Twomer_Seq[2,1:(Nuc_Length-1)]
  TwoMer=data.frame(rep(0,16))
  TwoMer=t(TwoMer)
  colnames(TwoMer)=Twomers_N
  TwoMer[1,]=sapply(Twomers_N, function(x) length(grep(x,Twomer_Seq)))
  TwoMer=rbind(TwoMer,0)
  TwoMer[2,]=TwoMer[1,]/(Nuc_Length-1)
  TwoMer=t(apply(TwoMer,1,function(x) round(x,digits = 2)))
  rownames(TwoMer)=c("Number","Percent")
  return(TwoMer)
}

