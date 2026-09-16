# DESCRIPTION has no Collate:, so R sources R/*.R in filename order and a
# second top-level definition of the same name silently wins with no error.
# Guard against reintroducing that failure mode (T25).

test_that("no function name is defined at package top level in more than one R/ file", {
  files <- list.files(testthat::test_path("../..", "R"), pattern = "[.]R$",
                      full.names = TRUE)
  if (!length(files)) skip("package sources not available")

  defs <- do.call(rbind, lapply(files, function(f) {
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (is.null(exprs)) return(NULL)
    nms <- vapply(as.list(exprs), function(e) {
      is_fun_assign <- is.call(e) && length(e) == 3 &&
        as.character(e[[1]]) %in% c("<-", "=") &&
        is.symbol(e[[2]]) &&
        is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
      if (is_fun_assign) as.character(e[[2]]) else NA_character_
    }, character(1))
    nms <- nms[!is.na(nms)]
    if (!length(nms)) return(NULL)
    data.frame(file = basename(f), name = nms, stringsAsFactors = FALSE)
  }))

  dup_names <- unique(defs$name[duplicated(defs$name)])
  dups <- defs[defs$name %in% dup_names, ]
  dups <- dups[order(dups$name, dups$file), ]

  expect_equal(
    dup_names, character(0),
    info = paste(capture.output(print(dups)), collapse = "\n")
  )
})
