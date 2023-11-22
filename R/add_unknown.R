#' Add unknowns to list of BUSCO genes
#' 
#' @param known_list A list of known BUSCO Sequences.
#' @param unknown_df A data.frame of BUSCO coordinates from a full_table.tsv
#' @param sample_name Name of the sample to add to Sequence names.
#' 
#' @returns A list.
#' 
#' @examples
#' DM6file <- system.file("extdata", "DM6_full_table.tsv.gz", package = 'BUSCOplot')
#' DM6 <- read_busco(DM6file)
#' chromL <- make_known(DM6, sample_name = "DM6:")
#' #lapply(chromL[1:2], head, n=2)
#' 
#' ATLfile <- system.file("extdata", "atlantic_full_table.tsv.gz", package = 'BUSCOplot')
#' ATL <- read_busco(ATLfile)
#' chromL <- add_unknown(known_list = chromL, unknown_df = ATL, sample_name = "ATL:")
#' #lapply(chromL[1:2], head, n=2)
#' 
#' 
#' 
#' @export
add_unknown <- function(known_list, unknown_df, sample_name = ""){
  unknown_df <- unknown_df[ unknown_df$Sequence != "Unplaced", ]
  unknown_df <- unknown_df[ unknown_df$Sequence != "", ]
  unknown_df$Sequence <- paste(sample_name, unknown_df$Sequence, sep = "")
  
  for(i in 1:length(known_list)){
    tmp <- unknown_df[ unknown_df$Busco_id %in% known_list[[i]]$Busco_id, ]
    #tmp[1:3, ]
    tmp <- tmp[ !duplicated(tmp$Busco_id), ]
    #tmp <- tmp[ tmp$Sequence != "Unplaced", ]
    #tmp <- tmp[ tmp$Sequence != "", ]
    #table(tmp$Sequence)
    #nrow(tmp)
    
    # Initialize an empty data structure.
    busco_chroms <- matrix(nrow = nrow(tmp), ncol = length(unique(tmp$Sequence)))
    busco_chroms <- as.data.frame(busco_chroms)
    rownames(busco_chroms) <- tmp$Busco_id
    colnames(busco_chroms) <- unique(tmp$Sequence)
    #busco_chroms[1:3, ]
    
    for(j in 1:ncol(busco_chroms)){
      busco_chroms[ tmp$Busco_id[ tmp$Sequence == colnames(busco_chroms)[j] ], j] <- tmp$Gene_Start[ tmp$Sequence == colnames(busco_chroms)[j] ]
    }
    known_list[[i]] <- cbind(
      known_list[[i]],
      busco_chroms[ known_list[[i]]$Busco_id, , drop = FALSE]
    )
  }
  
  return(known_list)
}
