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
#' @param inputId optional Shiny input id. When supplied the icon is a button
#'   that sends its 1-based row index to this input.
#'
#' @noRd
rt_dynamicIcon <- function(icons = NULL, labels = NULL, inputId = NULL) {
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
      var html = `<i class='${icon}'${name} ` +
        `style='padding-left: 0.2em;'></i>`;
      var clickid = '%s';
      if (!clickid) return html;
      return `<a href='#' class='grow mp-icon-btn' data-row='${cellInfo.index + 1}' ` +
        `onclick='event.preventDefault(); event.stopPropagation(); ` +
        `Shiny.setInputValue(&#39;${clickid}&#39;, this.dataset.row, {priority: &#39;event&#39;})'>` +
        html + `</a>`;
    }
    ",
    mp_js_obj(icons), mp_js_obj(labels), inputId %||% ""
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
      var raw = (cellInfo.value === null || cellInfo.value === undefined) ?
        '' : String(cellInfo.value)
      if (raw === '') return ''
      var text = raw.replace(/&/g, '&amp;').replace(/</g, '&lt;')
        .replace(/'/g, '&#39;')
      return `<abbr class='mp-abbr' tabindex='0' ` +
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
         out += `<span class='mp-override' title='Reference set manually; top BLAST hit was ${auto}'> *</span>`
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

#' Render a cell value as a status pill
#'
#' The one pill component. Colour carries the judgement, so the caller maps
#' each value to a meaning rather than to a colour.
#'
#' @param map named character vector, cell value -> one of "success",
#'   "warning", "danger", "neutral", "info".
#' @param labels optional named character vector, cell value -> display text.
#'   Defaults to the value itself.
#' @param empty text for a missing/blank value ("not set"). Use "" to render
#'   nothing at all.
#' @param hide character vector of values that render nothing.
#' @noRd
rt_pill <- function(map, labels = NULL, empty = "not set", hide = NULL) {
  hide_js <- if (length(hide) == 0) {
    "[]"
  } else {
    paste0("[", paste(sprintf("'%s'", mp_js_attr(hide)), collapse = ", "), "]")
  }
  sprintf(
    "function(cellInfo) {
      var map = %s;
      var labels = %s;
      var hide = %s;
      var empty = '%s';
      var v = cellInfo.value;
      var val = (v === null || v === undefined) ? '' : String(v);
      if (hide.indexOf(val) >= 0) return '';
      var tone, text;
      if (val === '') {
        if (empty === '') return '';
        tone = 'neutral';
        text = empty;
      } else {
        tone = map[val] || 'neutral';
        text = labels[val] || val;
      }
      return `<span class='mp-pill mp-pill-${tone}'>${text}</span>`;
    }",
    mp_js_obj(map), mp_js_obj(labels), hide_js, mp_js_attr(empty)
  ) |> htmlwidgets::JS()
}

#' Topology cell: the glyph and pill the annotation details header uses
#'
#' One pill per value, so a multi-scaffold "circular;linear" reads as two.
#' @noRd
rt_topology <- function() {
  htmlwidgets::JS("function(cellInfo) {
    var v = cellInfo.value;
    if (v === null || v === undefined || String(v).trim() === '') return '';
    return String(v).split(/[;,]\\s*/).map(function(t) {
      var circ = /circular$/.test(t);
      var glyph = circ ? '\\u21ba ' : (/linear$/.test(t) ? '\\u2194 ' : '');
      return `<span class='mp-pill mp-pill-${circ ? 'info' : 'neutral'}'>${glyph}${t}</span>`;
    }).join(' ');
  }")
}

#' Render a yes/no text column as a status pill
#'
#' "no" is grey in every column, never green; a blank value reads "not set".
#'
#' @param invert if TRUE, "yes" is the bad outcome and gets the red pill.
#' @param hide_no if TRUE, render an empty cell for "no"/NA values (only
#'   "yes" gets a pill). Useful for columns where "no" is the default
#'   and noisy to display.
#' @param yes_tone pill tone for "yes" when invert is FALSE; "warning" for a
#'   flag whose "yes" asks for attention (problematic, partial).
#' @noRd
rt_bool_badge <- function(invert = FALSE, hide_no = FALSE, yes_tone = "success") {
  rt_pill(
    map = c(yes = if (invert) "danger" else yes_tone, no = "neutral"),
    empty = if (hide_no) "" else "not set",
    hide = if (hide_no) "no" else NULL
  )
}

#' Render BLAST reference alignment status as a status pill
#'
#' States: good (green), poor (amber), failed (red), NULL/empty (none).
#'
#' @noRd
rt_blast_ref_status <- function() {
  rt_pill(
    c(good = "success", poor = "warning", failed = "danger"),
    empty = ""
  )
}

#' Add text click action to a cell
#'
#' @param InputId shiny input id to use
#' @param title optional tooltip naming the action. Without it the tooltip
#'   echoes the cell value, which is noise.
#' @noRd
rt_link <- function(InputId, title = NULL, lock_col = NULL) {
  tip <- if (is.null(title)) "${cellInfo.value}" else mp_js_attr(title)
  # On a locked row the cell is plain text: the click falls through to row
  # selection instead of silently doing nothing (theme T01).
  locked <- if (is.null(lock_col)) "" else sprintf(
    "var lk = (cellInfo.row || {})['%s'];
                if (lk == 1) {
                  return `<span class='mp-locked-cell' title='Locked - options are set to ${cellInfo.value}. Unlock this sample (Lock column) to change them.'>${cellInfo.value}</span>`;
                }",
    lock_col
  )
  sprintf(
    "function(cellInfo) {
                // An empty cell is not a link: nothing to click through to.
                if (cellInfo.value === null || cellInfo.value === undefined ||
                    cellInfo.value === '') { return ''; }
                %s
                var clickid = '%s';
                var sampid = cellInfo.index+1;
                return `<a href='#' id=${sampid} class='grow' title='%s' ` +
                `onclick='event.stopPropagation(); Shiny.onInputChange(&#39;${clickid}&#39;, this.id, {priority: &#39;event&#39;})'>` +
                cellInfo.value +
                `</a>`;
                }",
    locked, InputId, tip
  ) |>
    htmlwidgets::JS()
}

#' Column header with a tooltip
#'
#' Pass to `colDef(header = )` alongside the usual `name`, so the header keeps
#' its accessible name and gains a one-sentence explanation on hover.
#'
#' @param name header text
#' @param tip one sentence explaining the column. NULL returns plain text.
#' @noRd
rt_header <- function(name, tip = NULL) {
  if (is.null(tip) || !nzchar(tip)) {
    return(name)
  }
  htmltools::tags$span(class = "mp-th-tip", title = tip, name)
}

#' Format unix timestamp as a date in UI
#'
#' @noRd
rt_ts_date <- function() {
  htmlwidgets::JS(
    "
    function(cellInfo) {
      var options = { year: 'numeric', month: 'numeric', day: 'numeric', hour: 'numeric', minute: 'numeric' };
      var date = new Date(1000*cellInfo.value).toLocaleDateString(undefined, options);
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
#' @param disabled TRUE, or a reason string, renders the toggle inert
#'   (`disabled`, `aria-disabled`, no click handler). A disabled button gets
#'   no pointer events, so the reason rides on a wrapper span.
#'
#' @noRd
rt_bool_bttn <- function(inputId, ticon, ficon, title_true = NULL,
                         title_false = NULL, disabled = FALSE) {
  off <- !isFALSE(disabled)
  tips <- mp_js_obj(c(
    on = if (off || is.null(title_true)) "" else title_true,
    off = if (off || is.null(title_false)) "" else title_false
  ))
  gate <- if (off) {
    "disabled aria-disabled='true'"
  } else {
    sprintf(
      paste0("onclick='event.stopPropagation(); Shiny.setInputValue(",
             "&#39;%s&#39;, this.id, {priority: &#39;event&#39;})'"),
      inputId
    )
  }
  wrap <- if (off) {
    c(sprintf("<span title='%s'>",
              mp_js_attr(if (is.character(disabled)) disabled else "")),
      "</span>")
  } else {
    c("", "")
  }
  sprintf(
    "
    function(cellInfo) {
      var { index } = cellInfo;
      var on = cellInfo.value ? true : false;
      var icon = on ? '%s' : '%s';
      var tips = %s;
      var tip = on ? tips.on : tips.off;
      var t = tip ? ` title='${tip}'` : '';
      return `%s<button type='button' ` +
        `class='icon-bttn-text mp-toggle%s' ` +
        `id='${index+1}' aria-pressed='${on}'${t} %s>` +
        `<i class='${icon}' aria-hidden='true'></i>` +
        `</button>%s`
    }
    ",
    ticon, ficon, tips, wrap[1], if (off) "" else " grow", gate, wrap[2]
  ) |>
    htmlwidgets::JS()
}
