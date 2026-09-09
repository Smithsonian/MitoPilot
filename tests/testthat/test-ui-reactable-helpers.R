test_that("mp_js_attr escapes quotes and angle brackets", {
  expect_equal(mp_js_attr("it's <b>"), "it&#39;s &lt;b&gt;")
})

test_that("rt_dynamicIcon only names the glyph when labels are supplied", {
  bare <- as.character(rt_dynamicIcon(c(`0` = "fa fa-lock-open")))
  expect_true(grepl("var labels = {};", bare, fixed = TRUE))
  expect_false(grepl("reactable-btth", bare, fixed = TRUE))

  named <- as.character(rt_dynamicIcon(
    c(`0` = "fa fa-lock-open"),
    labels = c(`0` = "Unlocked")
  ))
  expect_true(grepl("role='img'", named, fixed = TRUE))
  expect_true(grepl("Unlocked", named, fixed = TRUE))
})

test_that("rt_icon_bttn_text keeps the cell value as the render gate", {
  js <- as.character(rt_icon_bttn_text("id", "fa fa-eye", "view",
                                       label = "Coverage", title = "Show it"))
  # The empty-value early return must survive relabelling, or buttons appear
  # on rows with nothing behind them.
  expect_true(grepl("value==='') {", js, fixed = TRUE))
  expect_lt(regexpr("return;", js, fixed = TRUE),
            regexpr("value = 'Coverage';", js, fixed = TRUE))
  expect_true(grepl("title='Show it'", js, fixed = TRUE))
  expect_true(grepl("type='button'", js, fixed = TRUE))
})

test_that("rt_bool_bttn renders a focusable button with aria-pressed", {
  js <- as.character(rt_bool_bttn("id", "fa fa-check", "fa fa-circle",
                                  "On now", "Off now"))
  expect_true(grepl("<button type='button'", js, fixed = TRUE))
  expect_true(grepl("aria-pressed=", js, fixed = TRUE))
  expect_true(grepl("On now", js, fixed = TRUE))
})

test_that("rt_bool_badge uses pill classes and never a green no", {
  js <- as.character(rt_bool_badge())
  expect_true(grepl("mp-pill", js, fixed = TRUE))
  expect_true(grepl("'yes': 'success'", js, fixed = TRUE))
  expect_true(grepl("'no': 'neutral'", js, fixed = TRUE))
  expect_true(grepl("not set", js, fixed = TRUE))

  hidden <- as.character(rt_bool_badge(hide_no = TRUE))
  expect_true(grepl("var hide = ['no']", hidden, fixed = TRUE))
})

test_that("rt_header returns plain text without a tip", {
  expect_identical(rt_header("Gene Order"), "Gene Order")
  expect_true(grepl(
    "mp-th-tip",
    as.character(rt_header("Gene Order", "How the genes are ordered")),
    fixed = TRUE
  ))
})

test_that("rt_longtext escapes the tooltip and skips empty cells", {
  js <- as.character(rt_longtext())
  expect_true(grepl("mp-abbr", js, fixed = TRUE))
  expect_true(grepl("if (raw === '') return ''", js, fixed = TRUE))
})
