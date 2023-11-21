#' Create list for BUSCO genes
#' 
#' @param x A matrix of BUSCO coordinates for one chromosome/scaffold.
#' 
#' @returns A list.
#' 
make_known <- function(x){
  x <- x[ x$Status == "Complete", , drop = FALSE]
  busco_l <- vector( mode = "list", length = length( unique(x$Sequence) ) )
  names(busco_l) <- sort(unique(x$Sequence), decreasing = FALSE)
  
  for( i in 1:length(busco_l) ){
    busco_l[[i]] <- x[ x$Sequence == names(busco_l)[i], ]
  }  
  return(busco_l)
}

