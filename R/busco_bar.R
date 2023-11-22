#' Create a gg style barplot for BUSCO genes
#' 
#' @param x A matrix of BUSCO coordinates for one chromosome/scaffold.
#' @param max_copy maximum copy number
#' 
#' @returns A ggplot object.
#' 
#' 
#' @examples
#' 
#' DM6file <- system.file("extdata", "DM6_full_table.tsv.gz", package = 'BUSCOplot')
#' DM6 <- read_busco(DM6file)
#' busco_bar(DM6)
#' 
#' 
#' @export
busco_bar <- function(x, max_copy = NULL){
  my_df <- as.data.frame( table( table( x$Busco_id[ x$Status %in% c("Complete", "Duplicated") ] ) ) )
  names(my_df) <- c("Copies", "Count")
  
  if( !is.null(max_copy) ){
    my_df$Count[ max_copy ] <- sum( my_df$Count[ as.numeric(as.character(my_df$Copies)) >= max_copy ] )
    my_df <- my_df[ 1:max_copy, ]
    my_df$Copies <- as.character(my_df$Copies)
    #my_df$Copies[ max_copy ] <- paste(">=", max_copy, sep = "")
    my_df$Copies[ max_copy ] <- paste(max_copy, "=<", sep = "")
  }
  
  my_df <- rbind(
    data.frame(
      Copies = c(0, "Fragmented"), 
      Count = c(sum(x$Status == "Missing"), sum(x$Status == "Fragmented"))
    ),
    my_df
  )
  my_df$Percent <- my_df$Count/sum(my_df$Count) * 100
  my_df$Copies <- factor(my_df$Copies, levels = my_df$Copies)
  
  #library(ggplot2)
  Copies <- Percent <- NULL
  p <- ggplot2::ggplot(data = my_df, mapping = ggplot2::aes( x = Copies, y = Percent ) )
  #p <- ggplot2::ggplot(data = my_df, mapping = ggplot2::aes( x = my_df$Copies, y = my_df$Percent ) )
  #p <- ggplot(data = my_df, mapping = aes( x = Copies, y = Count ) )
  p <- p + ggplot2::geom_bar(stat="identity", color="#000000", fill="#1E90FF")
  p <- p + ggplot2::theme_bw()
  #p <- p + ggtitle("BUSCO: DM6")
  return(p)
}
