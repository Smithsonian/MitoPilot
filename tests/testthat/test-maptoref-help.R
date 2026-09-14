test_that("mapper help shows only the selected mapper's icon and link", {
  s <- shiny::MockShinySession$new()
  html <- shiny::withReactiveDomain(s, as.character(shiny::tagList(
    mtr_mapper_help("bwa-mem", s$ns), mtr_mapper_help_text("bwa-mem", s$ns))))
  expect_match(html, 'id="mock-session-help_icon_bowtie2" class="shinyjs-hide"')
  expect_match(html, 'id="mock-session-help_icon_bwa-mem">')
  expect_match(html, "class=\"[^\"]*shinyjs-hide\" id=\"mock-session-help_maptoref_bowtie2\"")
  expect_match(html, "bio-bwa.sourceforge.net/bwa.shtml")
  expect_match(html, "bwa mem \\[options\\]")
  expect_match(html, "bowtie2 \\[options\\]")
})

test_that("bundled help exists for both mappers and samtools consensus", {
  for (t in c("bowtie2", "bwa-mem", "samtools-consensus")) {
    expect_false(grepl("not bundled", read_tool_help(t)), info = t)
  }
})
