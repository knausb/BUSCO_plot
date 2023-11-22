#' Create a line map using BUSCO coordinates
#' 
#' @param x A data.frame of BUSCO coordinates with Busco_ids as row.names.
#' @param alpha transparency or alpha channel
#' @param ... other options.
#' 
#' 
#' @returns An invisible NULL.
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
#' line_map(chromL[[1]])
#' 
#' 
#' @export
line_map <- function(x, alpha = 1, ...){
  #library(viridisLite)
  check_busco_table(x)
  if( length( unique( x$Sequence ) ) != 1 ){
    stop("Sequence column is expected to contain a single unique value.")
  }
  colnames(x)[4] <- unique( x$Sequence )
  x <- x[ , c(4, 9:ncol(x))]
  
  my_denom <- 1e6
  plot( x = c(1, ncol(x)),
        y = c(1, max(x, na.rm = TRUE)/my_denom), 
        type = "n",
        xlab = "", 
        #xlab = "Sample", 
        ylab = "Position (Mbp)",
        #ylab = "BUSCO Position (Mbp)",
        xaxt = "n", ...)
  graphics::axis( side = 1, at = 1:ncol(x), labels = colnames(x), las = 3 )
  
  my_ymax <- max(x, na.rm = TRUE) / my_denom
  
  grDevices::palette( viridisLite::magma(n=8, alpha = alpha) )
  for(i in 1:nrow(x)){
    graphics::lines( x = 1:ncol(x), 
           #         y = x[i, my_index]/my_denom, 
           y = x[i, ]/my_denom,          
           col = i
    )
    graphics::points( x = 1:ncol(x), 
            #         y = x[i, my_index]/my_denom, 
            y = x[i, ]/my_denom,          
            col = i, 
            pch = 20,
            cex = 0.6
    )
  }
  grDevices::palette( "default" )
  return(invisible(NULL))
}
