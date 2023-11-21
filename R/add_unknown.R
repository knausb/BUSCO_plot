#' Add unknowns to list of BUSCO genes
#' 
#' @param known_list A list of known BUSCOs.
#' @param unknown_df A data.frame of BUSCOs
#' 
#' @returns A list.
#' 
add_unknown <- function(known_list, unknown_df){
  for(i in 1:length(known_list)){
    tmp <- unknown_df[ unknown_df$Busco_id %in% known_list[[i]]$Busco_id, ]
    #tmp[1:3, ]
    tmp <- tmp[ !duplicated(tmp$Busco_id), ]
    tmp <- tmp[ tmp$Sequence != "Unplaced", ]
    tmp <- tmp[ tmp$Sequence != "", ]
    #table(tmp$Sequence)
    #nrow(tmp)
    
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
      busco_chroms[ known_list[[i]]$Busco_id, ]
    )
  }
  
  return(known_list)
}
