# The read-based and user-assembly apps are two hand-maintained copies of one
# shell, so every fix to one has silently missed the other (theme T15). These
# tests compare the two rendered shells directly, so the next drift fails here
# instead of in someone's project.

# Walk a tag tree and collect every node the predicate accepts.
collect_tags <- function(x, keep) {
  found <- list()
  walk <- function(node) {
    if (inherits(node, "shiny.tag")) {
      if (isTRUE(keep(node))) found[[length(found) + 1L]] <<- node
      lapply(node$children, walk)
    } else if (is.list(node)) {
      lapply(node, walk)
    }
    invisible(NULL)
  }
  walk(x)
  found
}

# Visible text of a tag, ignoring nested markup.
tag_text <- function(node) {
  bits <- character(0)
  walk <- function(n) {
    if (is.character(n)) {
      bits <<- c(bits, n)
    } else if (inherits(n, "shiny.tag")) {
      lapply(n$children, walk)
    } else if (is.list(n)) {
      lapply(n, walk)
    }
    invisible(NULL)
  }
  walk(node$children)
  trimws(paste(bits, collapse = " "))
}

# The shell's own toolbar buttons: top-level ids, so no module namespace.
toolbar_buttons <- function(ui) {
  btns <- collect_tags(ui, function(n) {
    identical(n$name, "button") &&
      !is.null(n$attribs$id) &&
      !grepl("-", n$attribs$id, fixed = TRUE)
  })
  out <- lapply(btns, function(n) {
    list(
      label = tag_text(n),
      icon = length(collect_tags(n, function(k) identical(k$name, "i"))) > 0,
      title = n$attribs$title %||% NA_character_,
      classes = sort(strsplit(trimws(n$attribs$class %||% ""), "\\s+")[[1]])
    )
  })
  names(out) <- vapply(btns, function(n) n$attribs$id, character(1))
  out[order(names(out))]
}

# Download links, by module-namespaced id.
download_links <- function(ui) {
  a <- collect_tags(ui, function(n) {
    identical(n$name, "a") && grepl("shiny-download-link", n$attribs$class %||% "", fixed = TRUE)
  })
  sort(vapply(a, function(n) n$attribs$id %||% "", character(1)))
}

reg <- toolbar_buttons(app_ui(NULL))
usr <- toolbar_buttons(app_ui_userAsmb(NULL))

test_that("both shells carry the same toolbar buttons", {
  expect_equal(names(reg), names(usr))
  expect_true("clear_group" %in% names(usr))
})

test_that("shared toolbar buttons have the same label, icon and emphasis", {
  for (id in intersect(names(reg), names(usr))) {
    expect_equal(reg[[id]]$label, usr[[id]]$label, info = id)
    expect_equal(reg[[id]]$icon, usr[[id]]$icon, info = id)
    expect_equal(reg[[id]]$classes, usr[[id]]$classes, info = id)
  }
})

test_that("every toolbar button names its action", {
  for (id in names(reg)) {
    expect_true(is.character(reg[[id]]$title) && nzchar(reg[[id]]$title), info = id)
  }
})

# Assemble and Annotate deliberately share the ids state, lock and run_modal:
# one gargoyle trigger serves whichever control group is on screen.
test_that("only the recommended action is emphasised", {
  primary <- names(reg)[vapply(reg, function(b) "btn-primary" %in% b$classes, logical(1))]
  expect_equal(sort(unique(primary)), c("export", "run_modal"))
  danger <- names(reg)[vapply(reg, function(b) "btn-danger" %in% b$classes, logical(1))]
  expect_equal(danger, character(0))
})

test_that("both Assemble tables offer the same CSV downloads", {
  usr_dl <- download_links(app_ui_userAsmb(NULL))
  skip_if(
    !any(grepl("^assemble-", usr_dl)),
    "userAsmb Assemble has no CSV download row yet (T15 item e, owned by W1-assemble)"
  )
  strip <- function(x) sort(sub("^[^-]+-", "", grep("^assemble-", x, value = TRUE)))
  expect_equal(strip(download_links(app_ui(NULL))), strip(usr_dl))
})
