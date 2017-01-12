#' @title SADEG.DimerStackingEnergy
#' @description Sum of Stacking energy of each Dimers.
#' @details Count the number of each Dimers and multiply by related stacking energy.
#' @author Babak Khorsand
#' @export SADEG.DimerStackingEnergy
#' @param Nucleotide_Sequence Nucleotide Sequence
#' @return Sum of Stacking energy of each Dimers.
#' @examples
#' SADEG.DimerStackingEnergy("actagtcacgatcag")
SADEG.DimerStackingEnergy = function(Nucleotide_Sequence)
{
  TwoMer=SADEG.Dimer(Nucleotide_Sequence)
  if (!is.null(TwoMer))
  {
    StackingEnergy_Twomers=StackingEnergy_Table[1:16]*TwoMer
    rownames(StackingEnergy_Twomers)=NULL
    colnames(StackingEnergy_Twomers)=paste(names(StackingEnergy_Table[1:16]),"StEn",sep = "_")
  }
  return(TwoMer)
}

