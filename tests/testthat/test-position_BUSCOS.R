
# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })


test_that("position_buscos works", {
  DM6file <- system.file("extdata", "DM6_full_table.tsv.gz", package = 'BUSCOplot')
  DM6 <- read_busco(DM6file)
  chromL <- make_known(DM6, sample_name = "DM6:")
  #lapply(chromL[1:2], head, n=2)
  ATLfile <- system.file("extdata", "atlantic_full_table.tsv.gz", package = 'BUSCOplot')
  ATL <- read_busco(ATLfile)
  chromL <- add_unknown(known_list = chromL, unknown_df = ATL, sample_name = "ATL:")
  #lapply(chromL[1:2], head, n=2)
 
  #gg_line_map(chromL[[1]])
  #chromL[[1]][1:3, ]
  tmp <- position_BUSCOS(chromL[[1]])
  
  expect_equal(ncol(tmp), 14)
  expect_equal(nrow(tmp), 329)
  
  testthat::expect_identical(
    c("ATL:chr01_3", "ATL:chr01_4", "ATL:chr01_0", "ATL:chr01_1", 
    "ATL:chr01_2", "ATL:chr03_1"),
    colnames(tmp)[-c(1:8)])
  
  
  #colnames(chromL[[1]])
  # test_order <- c(`ATL:chr01_3` = 314L, `ATL:chr01_4` = 326L, `ATL:chr01_0` = 231L, 
  #   `ATL:chr01_1` = 145L, `ATL:chr01_2` = 303L, `ATL:chr03_1` = 328L
  # )
  # check_order <- apply(tmp[ , 9:ncol(tmp)], MARGIN = 2, function(x){ max(x, na.rm = TRUE) })
  #check_order <- apply(tmp[ , 9:ncol(tmp)], MARGIN = 2, function(x){ sum(!is.na(x)) })
  
  #test_order == check_order  
})
 

test_that("position_buscos works, only one unknown sequence", {
  DM6file <- system.file("extdata", "DM6_full_table.tsv.gz", package = 'BUSCOplot')
  DM6 <- read_busco(DM6file)
  chromL <- make_known(DM6, sample_name = "DM6:")
  #lapply(chromL[1:2], head, n=2)
  ATLfile <- system.file("extdata", "atlantic_full_table.tsv.gz", package = 'BUSCOplot')
  ATL <- read_busco(ATLfile)
  chromL <- add_unknown(known_list = chromL, unknown_df = ATL, sample_name = "ATL:")
  #lapply(chromL[1:2], head, n=2)
  
  #gg_line_map(chromL[[1]])
  #chromL[[1]][1:3, ]
  tmp <- chromL[[1]]
  tmp <- tmp[, 1:9]
  tmp <- position_BUSCOS(tmp)
  
  expect_equal(ncol(tmp), 9)
  expect_equal(nrow(tmp), 329)
})


 
 