#' @title SADEG.CAI
#' @description Codon Adaption Index
#' @details Geometric mean of the RSCU values presented as relative adaptiveness value (w), The CAI for a gene is then defined as the geometric mean of w values for codons in that gene excluding methionine, tryptophan, and stop codons.
#' @author Babak Khorsand
#' @export SADEG.CAI
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return CAI
#' @examples
#' SADEG.CAI(Nucleotide_Sequence="atggctgctgcagcggccagtcacgatcagaggtaagttgtcgcagcatgt")
SADEG.CAI = function(Nucleotide_Sequence)
{
  RSCU_List=NULL
  Result=NULL
  Nucleotide_Sequence=toupper(Nucleotide_Sequence)
  Codon=sapply(0:((nchar(Nucleotide_Sequence)/3)-1), function(x) substr(Nucleotide_Sequence,(x*3)+1,(x*3)+3))
  Codon_Table=table(Codon)
  codon_List=strsplit(codon_List,",")
  RSCU_List=sapply(codon_List, function(x) sapply(x[-1], function(y) ifelse(is.na(Codon_Table[y]),0,Codon_Table[y])))
  for (i in 1:21)
  {
    names(RSCU_List[[i]])= codon_List[[i]][-1]
  }
  names(RSCU_List)=sapply(codon_List, function(x) x[1])
  RSCU_List=sapply(RSCU_List, function(x) {mean=mean(x); sapply(x, function(y) ifelse(mean==0,0,y/mean))})
  RSCU_Max=sapply(RSCU_List,max)
  W_List=mapply(function(x,y) x/y,x=RSCU_List,y=RSCU_Max)
  W_List=unlist(W_List)
  names(W_List)=gsub(".*\\.(...)","\\1",names(W_List))
  W_List=W_List[W_List>0]
  if (any(is.na(W_List))==T)
    W_List=W_List[-which(is.na(W_List))]
  Spare=c("ATG","TGG","TAG","TAA","TGA")
  L=sum(Codon_Table[-which(names(Codon_Table) %in% Spare)])
  Result=prod(W_List)^(1/L)
  names(Result)="CAI"
  if (Result>0)
    return(Result)
}
