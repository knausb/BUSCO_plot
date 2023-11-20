#' Read in a BUSCO full_table.tsv
#' 
#' @param file A *full_table.tsv file created by BUSCO.
#' 
#' 
#' @returns A data.frame.
#' 
read_busco <- function(x){
  busc <- utils::read.table(x, header = FALSE, sep = "\t", fill = TRUE)
  names(busc) <- c("Busco_id", "Status", "Sequence", "Gene_Start",
                   "Gene_End", "Strand", "Score", "Length")
  busc$Sequence[busc$Sequence == ""] <- "Unplaced"
  return(busc)
}
