#' Read in a BUSCO full_table.tsv
#' 
#' @param file A full_table.tsv file created by BUSCO.
#' 
#' 
#' @returns A data.frame.
#' 
#' @examples
#' system.file("DM6-full_table.tsv.gz", package = "BUSCOplot")
#' 
#' 
read_busco <- function(file){
  busc <- utils::read.table(file, header = FALSE, sep = "\t", fill = TRUE)
  names(busc) <- c("Busco_id", "Status", "Sequence", "Gene_Start",
                   "Gene_End", "Strand", "Score", "Length")
  busc$Sequence[busc$Sequence == ""] <- "Unplaced"
  return(busc)
}
