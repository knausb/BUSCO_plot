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
  my_header <- scan( file = file, what = "character", n = 10, sep = "\n", quiet = TRUE)
  my_header <- my_header[ grep("^# Busco id", my_header) ]
  my_header <- strsplit(my_header, split = "\t")
  my_header <- unlist(my_header)
  my_header <- sub("^# ", "", my_header)
  my_header <- sub("[[:blank:]]", "_", my_header)

  busc <- utils::read.table(file, header = FALSE, sep = "\t", fill = TRUE)
  # names(busc) <- c("Busco_id", "Status", "Sequence", "Gene_Start",
  #                  "Gene_End", "Strand", "Score", "Length")
  names(busc) <- my_header
  busc$Sequence[busc$Sequence == ""] <- "Unplaced"
  return(busc)
}
