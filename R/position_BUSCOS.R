#' Adjust the position and orientation of BUSCO sequences
#' 
#' @param x A data.frame of BUSCO coordinates with Busco_ids as row.names.
#' @param min_BUSCO minimum number of BUSCO genes per Sequence to retain it.
#' @param sort_by Sort the resulting BUSCO sequences
#' 
#' 
#' @returns A data.frame of BUSCO coordinates with Busco_ids as row.names.
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
#' 
#' gg_line_map(chromL[[1]])
#' tmp <- position_BUSCOS(chromL[[1]])
#' gg_line_map(tmp)
#' 
#' @export
position_BUSCOS <- function(x, min_BUSCO = 0, sort_by = c("max_pos", "max_busco")){
  check_busco_table(x)
  if( length( unique( x$Sequence ) ) != 1 ){
    stop("Sequence column is expected to contain a single unique value.")
  }
  #colnames(x)[4] <- unique( x$Sequence )
  #x <- x[ , c(4, 9:ncol(x))]  
  
  if( length(sort_by) > 1 ){
    sort_by <- sort_by[1]
  }
#  for(i in 1:length(x)){
#    tmp <- x[[i]]
    tmp <- x
    #tmp <- x[ , c(4, 9:ncol(x))]
    #tmp <- x[ , 9:ncol(x), drop = FALSE ]
    #tmp <- tmp[, apply(tmp, MARGIN = 2, function(x){ sum(!is.na(x)) }) > min_BUSCO]
    my_na <- apply(tmp[ , 9:ncol(tmp), drop = FALSE ], MARGIN = 2, function(x){ sum(!is.na(x)) })
    #tmp[ , 9:ncol(tmp), drop = FALSE ]
    tmp <- tmp[, c(rep(TRUE, times = 8), my_na >= min_BUSCO), drop = FALSE]
    
    tmp <- nudge_seqs(tmp, seed_col = 4, test_col = 9:ncol(tmp))
    tmp <- flip_seqs(tmp, seed_col = 4, test_col = 9:ncol(tmp))
    if( sort_by == "max_pos" ){
      tmp[ , 9:ncol(tmp)] <- tmp[ , sort.int( apply(tmp[ , 9:ncol(tmp)], MARGIN = 2, max, na.rm = TRUE), decreasing = TRUE, index.return = TRUE)$ix + 8]
    }
    if( sort_by == "max_busco" ){
      tmp[ , 9:ncol(tmp)] <- tmp[ , sort.int( apply(tmp[ , 9:ncol(tmp)], MARGIN = 2, function(x){ sum(!is.na(x)) }), decreasing = TRUE, index.return = TRUE)$ix + 8]
    }
    #tmp[1:3, ]
    #x[[i]] <- tmp
    x <- tmp
    #x <- cbind(x[, 1:8], tmp[, -1])
#  }
  return(x)
}


nudge_seqs <- function(x, seed_col = 1, test_col = 2:ncol(x)){
  for( i in test_col ){
    x2 <- x[, c( seed_col, i )]
    d1 <- sum(apply(x2, MARGIN = 1, function(x){ abs(x[1] - x[2]) }), na.rm = TRUE)
    nudge_size <- 0.5e6
    x2[, 2] <- x2[ , 2] + nudge_size
    d2 <- sum(apply(x2, MARGIN = 1, function(x){ abs(x[1] - x[2]) }), na.rm = TRUE)
    while( d2 < d1 ){
      d1 <- d2
      x2[, 2] <- x2[, 2] + nudge_size
      d2 <- sum(apply(x2, MARGIN = 1, function(x){ abs(x[1] - x[2]) }), na.rm = TRUE)
    }
    x[ , i] <- x2[, 2] - nudge_size
  }
  return(x)
}


flip_seqs <- function(x, seed_col = 1, test_col = 2:ncol(x)){
  #  for( i in 1:length(test_col) ){
  for( i in test_col ){
    x2 <- x[ , c(seed_col, i)]
    d1 <- sum(apply(x2, MARGIN = 1, function(x){ abs(x[1] - x[2]) }), na.rm = TRUE)
    #x2[, 2] <- max(x2[, 2], na.rm = TRUE) - x2[, 2]
    tmp <- x[, i]
    my_min <- min( tmp, na.rm = TRUE )
    tmp <- tmp - my_min
    tmp <- max(tmp, na.rm = TRUE) - tmp
    tmp <- tmp + my_min    
    d2 <- sum(apply(cbind(x2[, 1], tmp), MARGIN = 1, function(x){ abs(x[1] - x[2]) }), na.rm = TRUE)
    if( d2 < d1 ){
      # Flip
      #      tmp <- x[, i]
      #      my_min <- min( tmp, na.rm = TRUE )
      #      tmp <- tmp - my_min
      #      tmp <- max(tmp, na.rm = TRUE) - tmp
      #      tmp <- tmp + my_min
      x[, i] <- tmp
      colnames(x)[i] <- paste( colnames(x)[i], "_rev", sep = "")
      #x[, test_col] <-x2[ , 2]
    }
  }
  return(x)
}

