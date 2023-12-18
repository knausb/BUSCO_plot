
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


 
 