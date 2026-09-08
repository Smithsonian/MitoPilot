#' Highlight selected row(s)
#'
#' @noRd
rt_highlight_row <- function() {
  htmlwidgets::JS(
    "
    function(rowInfo) {
      if ( typeof rowInfo === 'undefined') return
      var col = rowInfo.selected ? 'var(--mp-primary-soft)' : 'var(--mp-surface)'
      return { background: col }
    }
    "
  )
}

#' Escape a label for interpolation into a single-quoted HTML attribute
#'
#' Every helper below builds HTML inside a JS template literal, so one
#' apostrophe or angle bracket in a caller-supplied label silently breaks the
#' cell.
#'
#' @noRd
mp_js_attr <- function(x) {
  htmltools::htmlEscape(as.character(x), attribute = TRUE)
}

#' Build a JS object literal from a named vector, values escaped
#'
#' @noRd
mp_js_obj <- function(x) {
  if (length(x) == 0) {
    return("{}")
  }
  paste0(
    "{ ",
    paste(sprintf("'%s': '%s'", names(x), mp_js_attr(x)), collapse = ", "),
    " }"
  )
}

#' Dynamic Icon
#'
#' @param icons a named character vector list of icons to use.
#' @param labels optional named character vector, keyed identically to
#'   `icons`, giving the accessible name of each state. When supplied the
#'   glyph gets `role="img"`, `aria-label` and `title`; without it the markup
#'   is unchanged.
#'
#' @noRd
rt_dynamicIcon <- function(icons = NULL, labels = NULL) {
  if (length(icons) == 0) {
    return({
      htmlwidgets::JS("function(cellInfo) {return cellInfo.value}")
    })
  }

  sprintf(
    "
    function(cellInfo){
      var { value } = cellInfo;
      var icons = %s;
      var labels = %s;
      var icon = icons[value] || '';
      var label = labels[value];
      var name = label ?
        ` role='img' aria-label='${label}' title='${label}'` : '';
      return `<i class='${icon}'${name} ` +
        `style='padding-left: 0.2em;'></i>`
    }
    ",
    mp_js_obj(icons), mp_js_obj(labels)
  ) |> htmlwidgets::JS()
}

#' Add hover info to truncated text in reactable
#'
#' Assumes that wrap=FALSE in the table so that long text is truncated.
#'
#' @noRd
rt_longtext <- function() {
  htmlwidgets::JS(
    "function(cellInfo) {
      var text = cellInfo.value ? cellInfo.value : ''
      return `<abbr style='cursor: info; text-decoration: none;' ` +
      `title='${text}'>${text}</abbr>`
    }"
  )
}

#' Render an NCBI GenBank accession as a clickable hyperlink
#'
#' @noRd
#' @param auto_col optional name of a sibling column holding the automatic
#'   (rank-1) accession. When the displayed value differs from it, append a
#'   subtle "*" marker with a tooltip naming the original top hit, so a manually
#'   set reference override is visible at a glance.
rt_ncbi_link <- function(auto_col = NULL) {
  marker <- if (is.null(auto_col)) {
    ""
  } else {
    sprintf(
      "var auto = cellInfo.row['%s'];
       if (auto && auto !== 'NO HIT' && auto !== text) {
         out += `<span title='manually set; top BLAST hit was ${auto}' style='color:#c07a00; font-weight:bold; cursor:help;'> *</span>`
       }",
      auto_col
    )
  }
  htmlwidgets::JS(sprintf(
    "function(cellInfo) {
      var text = cellInfo.value ? cellInfo.value : ''
      if (!text || text === 'NO HIT') return text
      var url = 'https://www.ncbi.nlm.nih.gov/nuccore/' + text
      var out = `<a href='${url}' target='_blank' rel='noopener noreferrer'>${text}</a>`
      %s
      return out
    }",
    marker
  ))
}

#' Render a yes/no text column as a colored badge
#'
#' @param invert if TRUE, "yes" is orange (bad) and "no" is green (good)
#' @param hide_no if TRUE, render an empty cell for "no"/NA values (only
#'   "yes" gets a badge). Useful for columns where "no" is the default
#'   and noisy to display.
#' @noRd
rt_bool_badge <- function(invert = FALSE, hide_no = FALSE) {
  yes_bg <- if (invert) "#fde8d0" else "#d4edda"
  yes_fg <- if (invert) "#7d4a1e" else "#2d6a4f"
  no_bg  <- if (invert) "#d4edda" else "#fde8d0"
  no_fg  <- if (invert) "#2d6a4f" else "#7d4a1e"
  sprintf(
    "function(cellInfo) {
      var val = cellInfo.value ? cellInfo.value : 'no'
      if (val !== 'yes' && %s) return ''
      var bg  = val === 'yes' ? '%s' : '%s'
      var fg  = val === 'yes' ? '%s' : '%s'
      return '<span style=\"background:' + bg + '; color:' + fg + '; border-radius:3px; ' +
             'padding:1px 6px; font-size:0.85em;\">' + val + '</span>'
    }",
    tolower(as.character(hide_no)), yes_bg, no_bg, yes_fg, no_fg
  ) |> htmlwidgets::JS()
}

#' Render BLAST reference alignment status as a colored badge
#'
#' States: good (green), poor (orange), failed (red), NULL/empty (none).
#'
#' @noRd
rt_blast_ref_status <- function() {
  htmlwidgets::JS(
    "function(cellInfo) {
      var val = cellInfo.value
      if (!val) return ''
      var bg, fg
      if (val === 'poor')   { bg = '#fde8d0'; fg = '#7d4a1e' }
      else if (val === 'failed') { bg = '#f5c6cb'; fg = '#721c24' }
      else if (val === 'good')   { bg = '#d4edda'; fg = '#2d6a4f' }
      else return val
      return '<span style=\"background:' + bg + '; color:' + fg + '; border-radius:3px; ' +
             'padding:1px 6px; font-size:0.85em;\">' + val + '</span>'
    }"
  )
}

#' Add text click action to a cell
#'
#' @param InputId shiny input id to use
#' @param title optional tooltip naming the action. Without it the tooltip
#'   echoes the cell value, which is noise.
#' @noRd
rt_link <- function(InputId, title = NULL) {
  tip <- if (is.null(title)) "${cellInfo.value}" else mp_js_attr(title)
  sprintf(
    "function(cellInfo) {
                // An empty cell is not a link: nothing to click through to.
                if (cellInfo.value === null || cellInfo.value === undefined ||
                    cellInfo.value === '') { return ''; }
                var clickid = '%s';
                var sampid = cellInfo.index+1;
                return `<a href='#' id=${sampid} class='grow' title='%s' ` +
                `onclick='event.stopPropagation(); Shiny.onInputChange(&#39;${clickid}&#39;, this.id, {priority: &#39;event&#39;})'>` +
                cellInfo.value +
                `</a>`;
                }",
    InputId, tip
  ) |>
    htmlwidgets::JS()
}

#' Format unix timestamp as a date in UI
#'
#' @noRd
rt_ts_date <- function() {
  htmlwidgets::JS(
    "
    function(cellInfo) {
      var options = { year: 'numeric', month: 'numeric', day: 'numeric', hour: 'numeric', minute: 'numeric' };
      var date = new Date(1000*cellInfo.value).toLocaleDateString('en-US', options);
      return date!=='Invalid Date' ? date : null;
    }
    "
  )
}

#' Add reactable icon button with text
#'
#' @param inputId shiny input id to use
#' @param icon font awesome icon name
#' @param text fallback label used when the cell value is empty. The cell
#'   value is still the render gate: an empty cell renders no button.
#' @param label optional explicit button text. When supplied it replaces the
#'   cell value in the label only, so rows with nothing behind them still
#'   render no button.
#' @param title optional tooltip naming the action.
#'
#' @noRd
rt_icon_bttn_text <- function(inputId, icon, text = "", label = NULL,
                              title = NULL) {
  relabel <- if (is.null(label)) {
    ""
  } else {
    sprintf("value = '%s';", mp_js_attr(label))
  }
  tip <- if (is.null(title)) {
    ""
  } else {
    sprintf(" title='%s'", mp_js_attr(title))
  }
  sprintf(
    "
    function(cellInfo) {
      var { index, value } = cellInfo;
      value = value ? value : '%s';
      if (value === undefined || value === null || value==='') {
        return;
      }
      %s
      return `<button type='button' ` +
        `class='icon-bttn-text grow' ` +
        `id='${index+1}'%s ` +
        `onclick='event.stopPropagation(); Shiny.setInputValue(&#39;%s&#39;, this.id, {priority: &#39;event&#39;})'>` +
        `<i class='%s' aria-hidden='true' ` +
        `style='margin-right: 4px;'></i>` +
        `<small>${value}</small>` +
        `</button>`
    }
    ",
    mp_js_attr(text), relabel, tip, inputId, icon
  ) |>
    htmlwidgets::JS()
}

#' Two-state in-table toggle
#'
#' Renders a real focusable button carrying `aria-pressed`, so the toggle is
#' reachable by keyboard and announces its state.
#'
#' @param inputId shiny input id to use
#' @param ticon font awesome icon shown when the value is true
#' @param ficon font awesome icon shown when the value is false
#' @param title_true,title_false tooltips for each state
#'
#' @noRd
rt_bool_bttn <- function(inputId, ticon, ficon, title_true = NULL,
                         title_false = NULL) {
  tips <- mp_js_obj(c(
    on = if (is.null(title_true)) "" else title_true,
    off = if (is.null(title_false)) "" else title_false
  ))
  sprintf(
    "
    function(cellInfo) {
      var { index } = cellInfo;
      var on = cellInfo.value ? true : false;
      var icon = on ? '%s' : '%s';
      var tips = %s;
      var tip = on ? tips.on : tips.off;
      var t = tip ? ` title='${tip}'` : '';
      return `<button type='button' ` +
        `class='icon-bttn-text mp-toggle grow' ` +
        `id='${index+1}' aria-pressed='${on}'${t} ` +
        `onclick='event.stopPropagation(); Shiny.setInputValue(&#39;%s&#39;, this.id, {priority: &#39;event&#39;})'>` +
        `<i class='${icon}' aria-hidden='true'></i>` +
        `</button>`
    }
    ",
    ticon, ficon, tips, inputId
  ) |>
    htmlwidgets::JS()
}
