#' Read in a BUSCO full_table.tsv
#' 
#' @param file A full_table.tsv file created by BUSCO.
#' 
#' 
#' @returns A data.frame.
#' 
#' @examples
#' DM6file <- system.file("extdata", "DM6_full_table.tsv.gz", package = 'BUSCOplot')
#' DM6 <- read_busco(DM6file)
#' 
#' @export
read_busco <- function(file){
  busc <- utils::read.table(file, header = FALSE, sep = "\t", fill = TRUE)
  names(busc) <- c("Busco_id", "Status", "Sequence", "Gene_Start",
                   "Gene_End", "Strand", "Score", "Length")
  busc$Sequence[busc$Sequence == ""] <- "Unplaced"
  return(busc)
}
