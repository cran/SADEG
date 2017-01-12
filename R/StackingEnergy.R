#' @title SADEG.StackingEnergy
#' @description Average free energy
#' @details Average free energy value was obtained via multiplying average energy of each dimer from each sequence in number of stacked bases. In order to designate and identify a product-favored process, minus sign was denoted before the average free energy (AFE) value. Lastly, all relevant stacked bases in each gene cumulatively defined the total stacking energy for respective sequences.
#' @author Babak Khorsand
#' @export SADEG.StackingEnergy
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return StackingEnergy
#' @examples
#' SADEG.StackingEnergy("actagtcacgatcag")
SADEG.StackingEnergy = function(Nucleotide_Sequence)
{
  Nucleotide_Sequence=toupper(Nucleotide_Sequence)
  Nuc_Length=nchar(Nucleotide_Sequence)
  StackingEnergy=sapply(1:(Nuc_Length-1), function(i) StackingEnergy_Table[substr(Nucleotide_Sequence,i,i+1)])
  ReverseComplement = SADEG.ReverseComplement(Nucleotide_Sequence)
  StackingEnergy = ifelse(substr(Nucleotide_Sequence,1,1) %in% c("A","T"),StackingEnergy_Table["init_AT"],StackingEnergy_Table["init_GC"])+
    ifelse(substr(Nucleotide_Sequence,Nuc_Length,Nuc_Length) %in% c("A","T"),StackingEnergy_Table["init_AT"],StackingEnergy_Table["init_GC"])+
    ifelse(Nucleotide_Sequence==ReverseComplement,StackingEnergy_Table["sym"],0)-
    sum(StackingEnergy)
  names(StackingEnergy)="StackingEnergy"
  return(StackingEnergy)
}
