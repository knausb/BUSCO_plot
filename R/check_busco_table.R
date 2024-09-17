#' Check a BUSCO table's format
#' 
#' @param x A data.frame of BUSCO coordinates from a full_table.tsv
#' 
#' @returns A logical or an ERROR.
#' 
#' 
#' @examples
#' DM6file <- system.file("extdata", "DM6_full_table.tsv.gz", package = 'BUSCOplot')
#' DM6 <- read_busco(DM6file)
#' chromL <- make_known(DM6, sample_name = "DM6:")
#' lapply(chromL[1:2], head, n=2)
#' 
#' 
#' @export
check_busco_table <- function(x){
  busco_names <- c("Busco_id", "Status", "Sequence", "Gene_Start",
                  "Gene_End", "Strand", "Score", "Length")
  # busco_names <- c("Busco id", "Status", "Sequence", "Gene Start",
  #                  "Gene End", "Strand", "Score", "Length")
  # busco_names <- c("Busco id", "Status", "Sequence", "Score", "Length")
  # colnames(x) <- sub("[[:blank:]]", "_", colnames(x))
  
  if( !all( busco_names %in% colnames(x) ) ){
    my_msg <- paste("Column names expected to include: ", 
                    paste(busco_names, collapse = ", "),
                    ".",
                    sep = "")
    stop( my_msg )    
  }
  
  # if( !all(colnames(x)[1:8] == busco_names) ){
  #   my_msg <- paste("First 8 column names expected to be: ", 
  #                   paste(busco_names, collapse = ", "),
  #                   ".",
  #                   sep = "")
  #   stop( my_msg )
  # }
  
  # return(x)
  return(TRUE)
}


