#' Distance based on difference in BUSCO coordinates
#' 
#' @param x A table of BUSCO coordinates.
#' @param perc_comp percent composition
#' 
#' 
#' @returns A matrix.
#' 
dist_fingerprint <- function(x, perc_comp = 0.2){
  if( ncol(x) == 1 ){
    return(0)
  }
  
  samp_l <- vector(mode = "list", length = ncol(x))
  names(samp_l) <- colnames(x)
  
  for(i in 1:ncol(x)){
    samp_l[[i]] <- stats::dist(x[, i, drop = FALSE])
  }
  #as.matrix(samp_l[[1]])[1:6, 1:4]
  
  # Create dist
  samp_dm <- matrix(ncol = ncol(x), nrow = ncol(x))
  colnames(samp_dm) <- colnames(x)
  rownames(samp_dm) <- colnames(x)
  #samp_dm[1:3, 1:3]
  
  for(i in 2:length(samp_l)){
    for(j in 1:(i-1)){
      my_weights <- 1:round(length(samp_l[[i]]) * perc_comp)
      samp_dm[ names(samp_l)[i], names(samp_l)[j] ] <- sum( sort(abs(samp_l[[i]] - samp_l[[j]]), decreasing = TRUE)[my_weights] , na.rm = TRUE)
      #      samp_dm[ names(samp_l)[i], names(samp_l)[j] ] <- sum( sort(abs(samp_l[[i]] - samp_l[[j]]), decreasing = TRUE)[1:50] , na.rm = TRUE)
      samp_dm[ names(samp_l)[j], names(samp_l)[i] ] <- samp_dm[ names(samp_l)[i], names(samp_l)[j] ]
      # samp_dm[ names(samp_l)[i], names(samp_l)[j] ] <- sum(abs(samp_l[[i]] - samp_l[[j]]), na.rm = TRUE)
      # samp_dm[ names(samp_l)[j], names(samp_l)[i] ] <- samp_dm[ names(samp_l)[i], names(samp_l)[j] ]
    }
  }
  diag(samp_dm) <- 0
  #samp_dm[1:5, 1:5]
  
  return(samp_dm)
}

