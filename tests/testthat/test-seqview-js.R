sv_page <- function() {
  skip_if_not_installed("chromote")
  skip_if(!nzchar(Sys.getenv("CHROMOTE_CHROME")) && is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
          "no Chrome for chromote")
  b <- chromote::ChromoteSession$new()
  withr::defer(b$close(), envir = parent.frame())
  page <- normalizePath(testthat::test_path("seqview", "index.html"))
  b$Page$navigate(paste0("file://", page))
  Sys.sleep(1)
  b
}
js <- function(b, code) b$Runtime$evaluate(code, returnByValue = TRUE)$result$value

test_that("lanes pack overlapping features and keep a wrapped feature in one lane", {
  b <- sv_page()
  n <- js(b, "(function(){var g=window.mpseq.geom; var f=[
    {pos1:10,pos2:100,dir:'+'},{pos1:90,pos2:200,dir:'+'},{pos1:150,pos2:160,dir:'-'},
    {pos1:16500,pos2:120,dir:'+'}]; var len=16600; var n=g.lanes(f,len);
    return JSON.stringify({n:n, lanes:f.map(function(x){return x.lane;})});})()")
  expect_equal(jsonlite::fromJSON(n), list(n = 3L, lanes = c(0L, 1L, 0L, 2L)))
})

test_that("codon centres follow the strand and wrap on a circular unit", {
  b <- sv_page()
  r <- js(b, "(function(){var g=window.mpseq.geom; var len=100;
    var plus={pos1:10,pos2:18,dir:'+'}, minus={pos1:10,pos2:18,dir:'-'}, wrap={pos1:98,pos2:6,dir:'+'};
    return JSON.stringify([g.codonCentre(plus,0,len,'linear'), g.codonCentre(plus,2,len,'linear'),
      g.codonCentre(minus,0,len,'linear'), g.codonCentre(minus,2,len,'linear'),
      g.codonCentre(wrap,0,len,'circular'), g.codonCentre(wrap,1,len,'circular'),
      g.nCodons(wrap,len), g.span(wrap,len)]);})()")
  expect_equal(jsonlite::fromJSON(r), c(11, 17, 17, 11, 99, 2, 3, 9))
})

test_that("the stop letter is drawn only for a stop codon beyond the translation", {
  b <- sv_page()
  r <- js(b, "(function(){var g=window.mpseq.geom; var len=1000;
    var withStop={pos1:1,pos2:9,dir:'+',translation:'MK'}, trimmed={pos1:1,pos2:9,dir:'+',translation:'MKL'};
    return JSON.stringify([g.stopLetter(withStop,2,len), g.stopLetter(withStop,1,len), g.stopLetter(trimmed,2,len)]);})()")
  expect_equal(jsonlite::fromJSON(r), c("*", "K", "L"))
})
