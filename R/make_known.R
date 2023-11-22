#' Create list for BUSCO Sequences
#' 
#' @param x A data.frame of BUSCO coordinates from a full_table.tsv
#' @param sample_name Name of the sample to add to Sequence names.
#' 
#' @returns A list.
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
make_known <- function(x, sample_name = ""){
  x <- x[ x$Status == "Complete", , drop = FALSE]
  x$Sequence <- paste(sample_name, x$Sequence, sep = "")
  busco_l <- vector( mode = "list", length = length( unique(x$Sequence) ) )
  names(busco_l) <- sort(unique(x$Sequence), decreasing = FALSE)
  
  for( i in 1:length(busco_l) ){
    busco_l[[i]] <- x[ x$Sequence == names(busco_l)[i], ]
  }  
  return(busco_l)
}

