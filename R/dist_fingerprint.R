#' Distance based on difference in BUSCO coordinates
#' 
#' 
#' @param x A data.frame of BUSCO coordinates with Busco_ids as row.names.
#' @param perc_comp percent of top comparisons to include \[0-1\].
#' 
#' @details
#' The input is reduced to a data.frame where each column is a sample and each row is the position for a BUSCO gene.
#' A matrix of distances between samples is created by subtracting pairs of among BUSCO matrices.
#' These differences are sorted and the first `perc_comp` percent of these comparisons are summed.
#' This sum is used as the distance among pairs of samples and is reported in the upper and lower triangles of a matrix.
#' This distance can be interpreted as the sum of the difference in lengths among pairs BUSCO genes.
#' 
#' 
#' @examples
#' DM6file <- system.file("extdata", "DM6_full_table.tsv.gz", package = 'BUSCOplot')
#' DM6 <- read_busco(DM6file)
#' chromL <- make_known(DM6, sample_name = "DM6:")
#' #lapply(chromL[1:2], head, n=2)
#' ATLfile <- system.file("extdata", "atlantic_full_table.tsv.gz", package = 'BUSCOplot')
#' ATL <- read_busco(ATLfile)
#' chromL <- add_unknown(known_list = chromL, unknown_df = ATL, sample_name = "ATL:")
#' #lapply(chromL[1:2], head, n=2)
#' dist_fingerprint(chromL[[1]])
#' 
#' 
#' @returns A matrix.
#' 
#' @export
dist_fingerprint <- function(x, perc_comp = 0.2){
  if( ncol(x) == 1 ){
    return(0)
  }
  check_busco_table(x)
  if( length( unique( x$Sequence ) ) != 1 ){
    stop("Sequence column is expected to contain a single unique value.")
  }
  colnames(x)[4] <- unique( x$Sequence )
  x <- x[ , c(4, 9:ncol(x)), drop = FALSE ]
  
  # Create a list where each element is sample.
  # Each element, or sample, consists of a distance matrix containing 
  # distances among pairs of BUSCO genes (columns and rows).
  samp_l <- vector(mode = "list", length = ncol(x))
  names(samp_l) <- colnames(x)
  for(i in 1:ncol(x)){
    samp_l[[i]] <- stats::dist(x[, i, drop = FALSE])
  }
  #as.matrix(samp_l[[1]])[1:6, 1:4]
  
  # Create matrix of distances.
  samp_dm <- matrix(ncol = ncol(x), nrow = ncol(x))
  colnames(samp_dm) <- colnames(x)
  rownames(samp_dm) <- colnames(x)
  diag(samp_dm) <- 0
  #samp_dm[1:3, 1:3]
  for(i in 2:length(samp_l)){
    for(j in 1:(i-1)){
      if( perc_comp > 0 ){
        my_weights <- 1:round(length(samp_l[[i]]) * perc_comp)
      } else {
        my_weights <- vector(length = 0)
      }
      samp_dm[ names(samp_l)[i], names(samp_l)[j] ] <- sum( sort(abs(samp_l[[i]] - samp_l[[j]]), decreasing = TRUE)[my_weights] , na.rm = TRUE)
      #      samp_dm[ names(samp_l)[i], names(samp_l)[j] ] <- sum( sort(abs(samp_l[[i]] - samp_l[[j]]), decreasing = TRUE)[1:50] , na.rm = TRUE)
      # Upper triangle.
      samp_dm[ names(samp_l)[j], names(samp_l)[i] ] <- samp_dm[ names(samp_l)[i], names(samp_l)[j] ]
      # 
      # samp_dm[ names(samp_l)[i], names(samp_l)[j] ] <- sum(abs(samp_l[[i]] - samp_l[[j]]), na.rm = TRUE)
      # samp_dm[ names(samp_l)[j], names(samp_l)[i] ] <- samp_dm[ names(samp_l)[i], names(samp_l)[j] ]
    }
  }
  #diag(samp_dm) <- 0
  #samp_dm[1:5, 1:5]
  
  return(samp_dm)
}

