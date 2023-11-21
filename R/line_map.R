#' Create a line map using BUSCO coordinates
#' 
#' @param x A table of BUSCO coordinates.
#' @param alpha transparency or alpha channel
#' @param ... other options.
#' 
#' 
#' @returns An invisible NULL.
#' 
line_map <- function(x, alpha = 1, ...){
  #library(viridisLite)
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
