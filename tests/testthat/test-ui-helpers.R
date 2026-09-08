# Shared UI helpers and vocabularies (dev/ui_review W0-3). These are the
# single source of truth for state words, column headers, modal footers and
# alerts, so a regression here shows up on every screen at once.

test_that("mp_n pluralises, and refuses to print NA", {
  expect_equal(mp_n(0, "sample"), "0 samples")
  expect_equal(mp_n(1, "sample"), "1 sample")
  expect_equal(mp_n(2, "sample"), "2 samples")
  expect_equal(mp_n(1, "assembly"), "1 assembly")
  expect_equal(mp_n(3, "assembly"), "3 assemblies")
  expect_equal(mp_n(2, "Taxon", plural = "Taxa"), "2 Taxa")
  expect_error(mp_n(NA, "sample"))
  expect_error(mp_n(NA_integer_, "sample"))
  expect_error(mp_n(c(1, 2), "sample"))
})

test_that("every module state table is complete and unambiguous", {
  for (m in names(MP_STATE_CODES)) {
    codes <- MP_STATE_CODES[[m]]
    expect_false(anyDuplicated(codes) > 0, info = m)
    expect_true(all(c("0", "1", "2", "3") %in% codes), info = m)
    expect_true(all(codes %in% names(MP_STATE_META)), info = m)
    for (f in c("label", "icon", "tip")) {
      v <- mp_state_field(m, f)
      expect_equal(length(v), length(codes), info = paste(m, f))
      expect_true(all(nzchar(v)), info = paste(m, f))
      expect_false(anyDuplicated(v) > 0, info = paste(m, f))
    }
  }
  # 4 is an Assemble-only, machine-set state.
  expect_true("4" %in% MP_STATE_CODES$assemble)
  expect_false("4" %in% MP_STATE_CODES$annotate)
  expect_false("4" %in% MP_STATE_SETTABLE)
  expect_true(all(MP_STATE_SETTABLE %in% MP_STATE_CODES$annotate))
  # Code 1 is "queued" in both modules; the same code never gets two words.
  expect_equal(unname(mp_state_labels("assemble")[["1"]]), "Ready to run")
  expect_equal(unname(mp_state_labels("annotate")[["1"]]), "Ready to run")
  expect_equal(mp_state_choices("annotate"),
               c(`On hold` = "0", `Ready to run` = "1", Success = "2", Failed = "3"))
  expect_error(mp_state_choices("export"))
})

test_that("column names and tips are one to one", {
  expect_false(anyDuplicated(names(MP_COL_NAMES)) > 0)
  expect_false(anyDuplicated(names(MP_COL_TIPS)) > 0)
  expect_true(all(names(MP_COL_TIPS) %in% names(MP_COL_NAMES)))
  expect_true(all(nzchar(MP_COL_NAMES)))
  expect_true(all(nzchar(MP_COL_TIPS)))
})

test_that("mp_footer puts the primary right-most", {
  f <- mp_footer(
    primary = actionButton("go", "Update"),
    danger = actionButton("rm", "Delete"),
    extra = actionButton("more", "Details")
  )
  html <- vapply(f, as.character, character(1))
  expect_length(html, 4)
  expect_match(html[1], ">Details<")
  expect_match(html[2], ">Delete<")
  expect_match(html[3], ">Cancel<")
  expect_match(html[4], ">Update<")
  expect_match(html[2], "btn-danger")
  expect_match(html[4], "btn-primary")
  expect_length(mp_footer(dismiss = NULL), 0)
  expect_length(mp_footer(primary = actionButton("go", "Update")), 2)
})

test_that("mp_toolbar_button carries its emphasis class and tooltip", {
  b <- as.character(mp_toolbar_button("lock", "Lock", emphasis = "danger",
                                      title = "Lock the selected samples"))
  expect_match(b, "mp-toolbar-btn")
  expect_match(b, "btn-danger")
  expect_false(grepl("btn-default", b))
  expect_match(b, "action-button")
  expect_match(b, 'title="Lock the selected samples"', fixed = TRUE)
  expect_false(grepl("aria-label", b))
  # icon-only controls get a matching aria-label
  icon_only <- as.character(mp_toolbar_button("refresh", "", title = "Reload the table"))
  expect_match(icon_only, 'aria-label="Reload the table"', fixed = TRUE)
  # a disabled button shows no tooltip, so the requirement goes on a wrapper
  gated <- as.character(mp_toolbar_button("state", "State", needs_selection = TRUE,
                                          title = "Set the state"))
  expect_match(gated, "mp-needs-selection-wrap")
  expect_match(gated, "Select one or more rows in the table first")
  expect_match(gated, "mp-needs-selection")
  expect_error(mp_toolbar_button("x", "X", emphasis = "success"))
})

test_that("mp_modal_title emits a dismiss control", {
  h <- as.character(mp_modal_title("Set state for 3 samples", subtitle = "SRR1, SRR2"))
  expect_match(h, 'data-dismiss="modal"', fixed = TRUE)
  expect_match(h, "modal-title")
  expect_match(h, "SRR1, SRR2", fixed = TRUE)
  expect_false(grepl("data-dismiss", as.character(mp_modal_title("Read only", close = FALSE))))
})

test_that("mp_alert refuses to render without a type", {
  expect_error(mp_alert("Export complete"))
  expect_error(mp_alert("Export complete", type = "done"))
})
