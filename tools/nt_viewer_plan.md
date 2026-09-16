# Sequence Viewer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A canvas viewer inside the annotation details window that shows the unit's nucleotide sequence, gene annotations in lanes, and one amino-acid row per protein-coding gene, and that follows every edit the window makes.

**Architecture:** R builds one JSON payload from the live annotation frame and the unit sequence and pushes it with a custom message; a single self-contained JavaScript file draws it on one canvas with semantic zoom and reports clicks back through a Shiny input. No library, no htmlwidget scaffold: the app already drives JavaScript through custom message handlers in `inst/app/www/custom.js`, and golem's `bundle_resources()` picks up every file under `inst/app/www`.

**Tech Stack:** R (shiny, gargoyle triggers, reactable, Biostrings), vanilla JavaScript on `<canvas>`, testthat 3 (+ withr), chromote for the JavaScript checks (skipped when unavailable), the `dev/ui_review` harness for in-app checks.

**Spec:** `tools/nt_viewer_spec.md` (approved 2026-09-12).

## Global Constraints

- Branch `seq-viewer` (off `map-to-ref-assembly` at 5a7a854). Never push; never add Claude attribution, `Co-Authored-By`, or a `Claude-Session` trailer to commits.
- ASCII only in R, JavaScript, and CSS. Minimal comments. Smallest working diff.
- Commit subjects: `feat(seqview): ...`, `test(seqview): ...`, `docs(seqview): ...`, `fix(annotate): ...`.
- UI rules already in force: labels end with a colon (`Position:`), buttons are `btn btn-default`, icon-only buttons carry `title` and `aria-label`, captions use `.mp-coverage-caption`, colours and spacing come from the `--mp-*` tokens in `inst/app/www/custom.css`, copy in sentence case.
- Coordinates: 1-based, inclusive; `pos1 > pos2` means the feature crosses the origin of a circular unit; strand is `"+"` / `"-"`; soft-deleted rows have `pos1 == 0`.
- Translation never happens in the browser; the stored `translation` column is the truth.
- Do not modify the annotation table's type-badge JS or the Coverage Map; do not touch `R/app_export.R`.
- Before every commit: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-variant-parity.R"); testthat::test_file("tests/testthat/test-no-duplicate-definitions.R")'` must pass, and every edited R file must parse.
- Apps for manual checks: `dev/ui_review/app.sh start ~/MitoPilot_scratch/new_test_project <port>` on ports 3856-3859 only; drive with `dev/ui_review/capture_lib.R` (see `dev/ui_review/HARNESS_README.md`); the details window opens with `click_in_row("<ID>", 2, rows = ROWS_ANN)` after `goto("Annotate")`; the headless viewport is about 540 px tall, so scroll the `.modal` element with `js()` to frame a region. Stop the app when done.

---

## File map

| File | Responsibility |
|---|---|
| `R/app_annotate_seqview.R` (new) | `seqview_payload()` (pure), `seqview_ui()`, `seqview_server()` |
| `inst/app/www/seqviewer.js` (new) | geometry (`window.mpseq.geom`), viewer class, message handlers, controls, tooltip, `window.mpseq.state()` |
| `inst/app/www/custom.css` | `:root` type-colour tokens; block "Sequence viewer" |
| `R/app_annotate_details.R` | mount the section in `annotate_details_modal()`; call `seqview_server()`; select a table row on pick |
| `R/annotate_utils.R` | `feature_nt()` (Task 6) |
| `tests/testthat/test-seqview-payload.R` (new) | payload unit tests |
| `tests/testthat/seqview/index.html` (new) | static page loading `seqviewer.js` with a Shiny shim |
| `tests/testthat/test-seqview-js.R` (new) | chromote-driven checks of geometry and viewer state (skipped without chromote) |
| `tests/testthat/test-feature-nt.R` (new) | Task 6 unit test |
| `dev/ui_review/steps/seqview_*.R` (new, gitignored) | in-app harness scripts |
| `NEWS.md`, `vignettes/Test-Project-Annotate.Rmd` | docs |

---

### Task 1: Payload builder (R, pure) with tests

**Files:**
- Create: `R/app_annotate_seqview.R`
- Test: `tests/testthat/test-seqview-payload.R`

**Interfaces:**
- Produces: `seqview_payload(annotations, seq, topology, unit, selected = NULL, version = 0L)` returning a plain list with fields `unit` (chr), `len` (int), `topology` (chr), `seq` (chr, upper case), `version` (int), `selected` (int or NULL), `features` (list of lists with `row`, `type`, `gene`, `pos1`, `pos2`, `dir`, `partial5`, `partial3`, `notes`, and optionally `translation`, `joined`). Consumed by Task 4 and mirrored by the JavaScript in Task 2.

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test-seqview-payload.R
sv_ann <- function(...) {
  base <- data.frame(
    type = "PCG", gene = "nad1", pos1 = 10L, pos2 = 39L, direction = "+",
    partial_start = "no", partial_stop = "no", translation = "MKLIVLLKN",
    notes = "", stringsAsFactors = FALSE
  )
  rows <- list(...)
  if (length(rows) == 0) return(base)
  do.call(rbind, lapply(rows, function(r) { b <- base; b[names(r)] <- r; b }))
}

test_that("payload carries the unit, sequence, and every live feature", {
  a <- sv_ann(list(), list(gene = "trnF", type = "tRNA", pos1 = 40L, pos2 = 108L, translation = NA))
  p <- seqview_payload(a, "acgtacgt", "circular", "S1.1.1", selected = 2L, version = 3L)
  expect_equal(p$unit, "S1.1.1")
  expect_equal(p$len, 8L)
  expect_equal(p$seq, "ACGTACGT")
  expect_equal(p$topology, "circular")
  expect_equal(p$version, 3L)
  expect_equal(p$selected, 2L)
  expect_length(p$features, 2)
  f <- p$features[[1]]
  expect_equal(f[c("row", "gene", "pos1", "pos2", "dir")],
               list(row = 1L, gene = "nad1", pos1 = 10L, pos2 = 39L, dir = "+"))
  expect_equal(f$translation, "MKLIVLLKN")
  expect_false(f$partial5); expect_false(f$partial3)
  expect_null(p$features[[2]]$translation)
})

test_that("soft-deleted rows are dropped and the selection follows the row index", {
  a <- sv_ann(list(), list(gene = "nad2_DELETED_1", pos1 = 0L, pos2 = 0L),
              list(gene = "cox1", pos1 = 100L, pos2 = 200L))
  p <- seqview_payload(a, "ACGT", "linear", "S1.1.1", selected = 3L)
  expect_equal(vapply(p$features, `[[`, integer(1), "row"), c(1L, 3L))
  expect_equal(p$selected, 3L)
  expect_null(seqview_payload(a, "ACGT", "linear", "S1.1.1", selected = 2L)$selected)
})

test_that("partial flags, join markers, and notes are carried", {
  a <- sv_ann(list(partial_start = "yes", notes = "JOIN: mode=exon group=2 extra words that go on and on and on and on and on and on and on"))
  f <- seqview_payload(a, "ACGT", "linear", "S1.1.1")$features[[1]]
  expect_true(f$partial5); expect_false(f$partial3)
  expect_equal(f$joined, "JOIN: mode=exon group=2")
  expect_equal(nchar(f$notes), 80L)
})

test_that("a wrap-around feature is passed through untouched", {
  a <- sv_ann(list(pos1 = 16500L, pos2 = 120L))
  f <- seqview_payload(a, strrep("A", 16600), "circular", "S1.1.1")$features[[1]]
  expect_equal(c(f$pos1, f$pos2), c(16500L, 120L))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-seqview-payload.R")'`
Expected: FAIL with `could not find function "seqview_payload"`.

- [ ] **Step 3: Write the payload builder**

```r
# R/app_annotate_seqview.R

#' Sequence viewer payload (tools/nt_viewer_spec.md, section 4)
#'
#' Pure: no Shiny, no database. Soft-deleted rows (pos1 == 0) are dropped;
#' the browser owns all coordinate arithmetic, so positions pass through as
#' stored (1-based, inclusive, pos1 > pos2 for an origin-crossing feature).
#' @noRd
seqview_payload <- function(annotations, seq, topology, unit,
                            selected = NULL, version = 0L) {
  seq <- toupper(as.character(seq)[1])
  a <- annotations
  yes <- function(x) {
    x <- tolower(as.character(x))
    length(x) == 1L && !is.na(x) && x %in% c("yes", "true", "1")
  }
  rows <- which(!is.na(a$pos1) & a$pos1 > 0)
  feats <- lapply(rows, function(i) {
    notes <- as.character(a$notes[i])
    if (is.na(notes)) notes <- ""
    joined <- regmatches(notes, regexpr("JOIN: mode=[A-Za-z]+ group=[0-9]+", notes))
    tr <- as.character(a$translation[i])
    f <- list(
      row = i,
      type = as.character(a$type[i]),
      gene = as.character(a$gene[i]),
      pos1 = as.integer(a$pos1[i]),
      pos2 = as.integer(a$pos2[i]),
      dir = as.character(a$direction[i]),
      partial5 = yes(a$partial_start[i]),
      partial3 = yes(a$partial_stop[i]),
      notes = substr(notes, 1L, 80L)
    )
    if (identical(f$type, "PCG") && !is.na(tr) && nzchar(tr)) f$translation <- tr
    if (length(joined) == 1L) f$joined <- joined
    f
  })
  sel <- if (length(selected) == 1L && !is.na(selected) && selected %in% rows) {
    as.integer(selected)
  }
  list(
    unit = unit, len = nchar(seq), topology = topology, seq = seq,
    version = as.integer(version), selected = sel, features = feats
  )
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-seqview-payload.R")'`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
git add R/app_annotate_seqview.R tests/testthat/test-seqview-payload.R
git commit -m "feat(seqview): payload builder for the sequence viewer"
```

---

### Task 2: JavaScript geometry with a static test page

**Files:**
- Create: `inst/app/www/seqviewer.js` (geometry part; Task 3 adds the viewer)
- Create: `tests/testthat/seqview/index.html`
- Test: `tests/testthat/test-seqview-js.R`

**Interfaces:**
- Produces `window.mpseq.geom` with `span(f, len)`, `lanes(feats, len)` (assigns `f.lane`, returns the lane count), `nCodons(f, len)`, `codonCentre(f, i, len, topology)`, `stopLetter(f, i, len)`. Task 3 draws with these.

- [ ] **Step 1: Write the static page and the failing R test**

```html
<!-- tests/testthat/seqview/index.html -->
<!doctype html><meta charset="utf-8"><title>seqviewer test</title>
<script>
  // Shiny shim: capture handlers and inputs so the viewer can be driven by hand.
  window.__handlers = {}; window.__inputs = [];
  window.Shiny = {
    addCustomMessageHandler: function (name, fn) { window.__handlers[name] = fn; },
    setInputValue: function (name, value) { window.__inputs.push({ name: name, value: value }); }
  };
</script>
<div style="width:1000px">
  <details id="sv-section" open><summary>Sequence</summary>
    <div class="mp-seqview-controls">
      <input type="checkbox" id="sv-show_nt" checked>
      <input type="checkbox" id="sv-show_aa" checked>
      <input type="number" id="sv-goto">
      <button type="button" data-mpseq="zoom_in">+</button>
      <button type="button" data-mpseq="zoom_out">-</button>
      <button type="button" data-mpseq="whole">Whole</button>
      <button type="button" data-mpseq="fit">Fit</button>
    </div>
    <div class="mp-seqview"><canvas id="sv-canvas"></canvas><div id="sv-tip" hidden></div></div>
  </details>
</div>
<script src="../../../inst/app/www/seqviewer.js"></script>
```

```r
# tests/testthat/test-seqview-js.R
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
```

Why `[0, 1, 0, 2]`: sorted by `pos1` the order is 10-100, 90-200, 150-160, 16500-120. The first takes lane 0; 90-200 overlaps it, lane 1; 150-160 clears lane 0 (its last end 100 is before 150), so lane 0; the wrapped feature linearises to 16500-16720 and its tail `[1, 120]` touches lane 0 (10-100) and lane 1 (90-200), so it needs lane 2. Three lanes.

- [ ] **Step 2: Run the R test to verify it fails**

Run: `CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-seqview-js.R")'`
Expected: FAIL (or ERROR) because `window.mpseq` is undefined. If it is SKIPPED, chromote is not usable on this machine: run the checks by hand in a browser console and say so in the commit body.

- [ ] **Step 3: Write the geometry module**

```js
// inst/app/www/seqviewer.js
// Sequence viewer for the annotation details window (tools/nt_viewer_spec.md).
(function () {
  'use strict';

  // ---- geometry: pure functions, testable without a canvas ----
  function span(f, len) {
    return f.pos1 <= f.pos2 ? f.pos2 - f.pos1 + 1 : len - f.pos1 + 1 + f.pos2;
  }
  function wraps(f) { return f.pos1 > f.pos2; }
  // Greedy interval packing on linearised coordinates; a wrapped feature also
  // claims [1, pos2], so it is checked against both ends of the circle.
  function lanes(feats, len) {
    var order = feats.slice().sort(function (a, b) { return a.pos1 - b.pos1; });
    var laneEnds = [];   // last linearised end per lane
    var laneHeads = [];  // last [1, pos2] head per lane (wrapped features)
    order.forEach(function (f) {
      var end = f.pos1 + span(f, len) - 1;
      var head = wraps(f) ? f.pos2 : 0;
      var lane = -1;
      for (var i = 0; i < laneEnds.length; i++) {
        var free = laneEnds[i] < f.pos1 && laneHeads[i] < f.pos1;
        if (free && head > 0) {
          // the tail [1, head] must not touch anything already in this lane
          var clash = order.some(function (g) {
            return g.lane === i && g !== f && g.pos1 <= head;
          });
          free = !clash;
        }
        if (free) { lane = i; break; }
      }
      if (lane < 0) { lane = laneEnds.length; laneEnds.push(0); laneHeads.push(0); }
      laneEnds[lane] = Math.max(laneEnds[lane], end);
      laneHeads[lane] = Math.max(laneHeads[lane], head);
      f.lane = lane;
    });
    return laneEnds.length;
  }
  function nCodons(f, len) { return Math.floor(span(f, len) / 3); }
  // 1-based position of the middle base of codon i (0-based).
  function codonCentre(f, i, len, topology) {
    var p = f.dir === '-' ? f.pos2 - 3 * i - 1 : f.pos1 + 3 * i + 1;
    if (topology === 'circular') p = ((p - 1) % len + len) % len + 1;
    return p;
  }
  // Letter for codon i: the stored translation, "*" for a trailing stop codon
  // the translation does not include, nothing otherwise.
  function stopLetter(f, i, len) {
    var tr = f.translation || '';
    if (i < tr.length) return tr.charAt(i);
    var n = nCodons(f, len);
    return (i === n - 1 && tr.length === n - 1) ? '*' : '';
  }

  window.mpseq = window.mpseq || {};
  window.mpseq.geom = { span: span, wraps: wraps, lanes: lanes, nCodons: nCodons,
                        codonCentre: codonCentre, stopLetter: stopLetter };
})();
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-seqview-js.R")'`
Expected: PASS, 3 tests (lanes `[0, 1, 0, 2]`, n = 3).

- [ ] **Step 5: Commit**

```bash
git add inst/app/www/seqviewer.js tests/testthat/seqview/index.html tests/testthat/test-seqview-js.R
git commit -m "feat(seqview): lane packing and codon geometry with a browser test"
```

---

### Task 3: Canvas viewer (draw, zoom, pan, hover, messages, controls)

**Files:**
- Modify: `inst/app/www/seqviewer.js` (append the viewer after the geometry block)
- Modify: `inst/app/www/custom.css` (append `:root` type tokens and the "Sequence viewer" block)
- Test: `tests/testthat/test-seqview-js.R` (append two tests)

**Interfaces:**
- Consumes `window.mpseq.geom` (Task 2) and a payload shaped as in Task 1 plus `id` (canvas element id) and `input` (Shiny input id for picks).
- Produces message handlers `mpseq` (load payload) and `mpseq_select` (`{id, row}`), Shiny input `<input>` with `{row, nonce}` on a gene click, `window.mpseq.state(id)` returning `{len, version, topology, viewStart, ppb, selected, nLanes, features:[{row,gene,pos1,pos2,lane}]}`, and `window.mpseq.zoom(id, factor)`, `window.mpseq.fit(id, row)`, `window.mpseq.whole(id)`, `window.mpseq.goto(id, pos)` for the harness.

- [ ] **Step 1: Append the failing tests**

```r
test_that("the viewer loads a payload, fits a gene, and reports clicks", {
  b <- sv_page()
  r <- js(b, "(function(){
    var seq = Array(2000).join('ACGT').slice(0, 2000);
    window.__handlers.mpseq({id:'sv-canvas', input:'sv-pick', unit:'S1.1.1', len:2000, topology:'circular', seq:seq, version:1,
      selected:null, features:[{row:1,type:'PCG',gene:'nad1',pos1:100,pos2:399,dir:'+',partial5:false,partial3:false,notes:'',translation:'MKL'},
                               {row:3,type:'tRNA',gene:'trnF',pos1:380,pos2:450,dir:'-',partial5:false,partial3:false,notes:''}]});
    var s0 = window.mpseq.state('sv-canvas');
    window.__handlers.mpseq_select({id:'sv-canvas', row:1});
    var s1 = window.mpseq.state('sv-canvas');
    window.mpseq.whole('sv-canvas'); var s2 = window.mpseq.state('sv-canvas');
    window.mpseq.zoom('sv-canvas', 2); var s3 = window.mpseq.state('sv-canvas');
    window.mpseq.goto('sv-canvas', 1990); var s4 = window.mpseq.state('sv-canvas');
    return JSON.stringify({n:s0.features.length, lanes:s0.nLanes, sel:s1.selected, fitStart:s1.viewStart, fitPpb:s1.ppb,
      wholePpb:s2.ppb, zoomPpb:s3.ppb, gotoStart:s4.viewStart, inputs:window.__inputs.length});})()")
  s <- jsonlite::fromJSON(r)
  expect_equal(s$n, 2); expect_equal(s$lanes, 2); expect_equal(s$sel, 1)
  expect_lt(s$fitStart, 100); expect_gt(s$fitPpb, 1)
  expect_equal(round(s$wholePpb * 2000), 1000)   # canvas is 1000 px wide in the page
  expect_equal(round(s$zoomPpb / s$wholePpb), 2)
  # centred on 1990 the view starts before the origin on a circular unit
  expect_gt(s$gotoStart, 1000)
})

test_that("a click on a gene arrow sends the row through the Shiny input", {
  b <- sv_page()
  r <- js(b, "(function(){
    window.__inputs = [];
    var seq = Array(2000).join('ACGT').slice(0, 2000);
    window.__handlers.mpseq({id:'sv-canvas', input:'sv-pick', unit:'S1.1.1', len:2000, topology:'linear', seq:seq, version:1,
      selected:null, features:[{row:2,type:'PCG',gene:'cox1',pos1:1,pos2:2000,dir:'+',partial5:false,partial3:false,notes:'',translation:'M'}]});
    window.mpseq.whole('sv-canvas');
    var hit = window.mpseq.hitTest('sv-canvas', 500, window.mpseq.laneY('sv-canvas', 0));
    window.mpseq.click('sv-canvas', 500, window.mpseq.laneY('sv-canvas', 0));
    return JSON.stringify({hit: hit && hit.row, sent: window.__inputs.map(function(i){return i.name+':'+i.value.row;})});})()")
  s <- jsonlite::fromJSON(r)
  expect_equal(s$hit, 2)
  expect_equal(s$sent, "sv-pick:2")
})
```

- [ ] **Step 2: Run the tests to verify the new ones fail**

Run: `CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-seqview-js.R")'`
Expected: the two new tests FAIL (`window.__handlers.mpseq is not a function`); the three geometry tests still pass.

- [ ] **Step 3: Append the viewer to seqviewer.js**

```js
(function () {
  'use strict';
  var G = window.mpseq.geom;
  var MAX_PPB = 14, NT_LETTER = 8, NT_BAR = 3, AA_MIN = 4;
  var RULER_H = 22, LANE_H = 22, NT_H = 20, AA_H = 20, GUTTER = 60, PAD = 4;
  var BASE = { A: '#3aa03a', C: '#2f6fb5', G: '#e0a030', T: '#cc4b4b', N: '#999999' };
  var viewers = {};

  function cssVar(name, fallback) {
    var v = getComputedStyle(document.documentElement).getPropertyValue(name).trim();
    return v || fallback;
  }
  function typeColor(t) { return cssVar('--mp-type-' + String(t || '').toLowerCase(), '#888888'); }
  function niceStep(raw) {
    var p = Math.pow(10, Math.floor(Math.log10(raw)));
    var m = raw / p;
    return (m <= 1 ? 1 : m <= 2 ? 2 : m <= 5 ? 5 : 10) * p;
  }

  function Viewer(id) {
    this.id = id;
    this.canvas = document.getElementById(id);
    this.wrap = this.canvas.parentElement;
    this.tip = document.getElementById(id.replace(/-canvas$/, '-tip'));
    this.section = this.canvas.closest('details');
    this.len = 0; this.seq = ''; this.feats = []; this.topology = 'linear';
    this.version = null; this.selected = null; this.nLanes = 0;
    this.viewStart = 1; this.ppb = 1;
    this.showNt = true; this.showAa = true;
    this.hits = []; this.aaRows = [];
    this.bindControls();
    var self = this;
    if (window.ResizeObserver) new ResizeObserver(function () { self.draw(); }).observe(this.wrap);
    if (this.section) this.section.addEventListener('toggle', function () { self.draw(); });
  }

  Viewer.prototype.load = function (p) {
    var sameSeq = (this.version === p.version && this.len === p.len);
    this.unit = p.unit; this.len = p.len; this.seq = p.seq || ''; this.topology = p.topology;
    this.version = p.version; this.input = p.input;
    this.feats = (p.features || []).map(function (f) { return Object.assign({}, f); });
    this.nLanes = G.lanes(this.feats, this.len);
    if (p.selected !== undefined && p.selected !== null) this.selected = p.selected;
    else if (!this.feats.some(function (f) { return f.row === this.selected; }, this)) this.selected = null;
    if (!sameSeq) this.whole();
    this.draw();
  };

  // ---- view state ----
  Viewer.prototype.width = function () { return Math.max(200, this.wrap.clientWidth - GUTTER); };
  Viewer.prototype.minPpb = function () { return this.width() / this.len; };
  Viewer.prototype.viewLen = function () { return this.width() / this.ppb; };
  Viewer.prototype.clamp = function () {
    this.ppb = Math.min(MAX_PPB, Math.max(this.minPpb(), this.ppb));
    if (this.topology === 'linear') {
      this.viewStart = Math.max(1, Math.min(this.len - this.viewLen() + 1, this.viewStart));
    } else {
      this.viewStart = ((this.viewStart - 1) % this.len + this.len) % this.len + 1;
    }
  };
  Viewer.prototype.whole = function () { this.ppb = this.minPpb(); this.viewStart = 1; this.clamp(); this.draw(); };
  Viewer.prototype.zoom = function (factor, atPos) {
    var centre = atPos || (this.viewStart + this.viewLen() / 2);
    var newPpb = Math.min(MAX_PPB, Math.max(this.minPpb(), this.ppb * factor));
    this.viewStart = centre - (centre - this.viewStart) * (this.ppb / newPpb);
    this.ppb = newPpb; this.clamp(); this.draw();
  };
  Viewer.prototype.goto = function (pos) {
    if (this.ppb < NT_LETTER) this.ppb = 10;
    this.viewStart = pos - this.viewLen() / 2; this.clamp(); this.draw();
  };
  Viewer.prototype.fit = function (row) {
    var f = this.feats.find(function (x) { return x.row === row; });
    if (!f) return;
    this.selected = row;
    var s = G.span(f, this.len), margin = Math.max(3, s * 0.05);
    this.ppb = Math.min(MAX_PPB, this.width() / (s + 2 * margin));
    this.viewStart = f.pos1 - margin; this.clamp();
    if (this.section && !this.section.open) this.section.open = true;
    this.draw();
  };

  // ---- coordinate helpers (linearised: a position may exceed len when wrapped) ----
  Viewer.prototype.x = function (lin) { return GUTTER + (lin - this.viewStart) * this.ppb; };
  Viewer.prototype.segments = function (f) {
    var s = G.span(f, this.len), out = [], vs = this.viewStart, ve = this.viewStart + this.viewLen();
    var ks = this.topology === 'circular' ? [-1, 0, 1] : [0];
    ks.forEach(function (k) {
      var a = f.pos1 + k * this.len, b = a + s - 1;
      if (b >= vs && a <= ve) out.push([a, b, k]);
    }, this);
    return out;
  };
  Viewer.prototype.baseAt = function (lin) {
    var p = this.topology === 'circular' ? ((lin - 1) % this.len + this.len) % this.len + 1 : lin;
    if (p < 1 || p > this.len) return null;
    return { pos: p, base: this.seq.charAt(p - 1) || 'N' };
  };

  // ---- drawing ----
  Viewer.prototype.laneY = function (lane) { return RULER_H + PAD + lane * LANE_H; };
  Viewer.prototype.height = function () {
    var pcgs = this.showAa && this.ppb >= AA_MIN ? this.aaRows.length : 0;
    return RULER_H + PAD + this.nLanes * LANE_H + PAD + (this.showNt && this.ppb >= NT_BAR ? NT_H : 0) + pcgs * AA_H + PAD;
  };
  Viewer.prototype.draw = function () {
    if (!this.len || !this.canvas.offsetParent) return;
    var dpr = window.devicePixelRatio || 1, W = this.wrap.clientWidth;
    this.aaRows = this.showAa && this.ppb >= AA_MIN
      ? this.feats.filter(function (f) { return f.type === 'PCG' && f.translation !== undefined && this.segments(f).length; }, this)
      : [];
    var H = this.height();
    this.canvas.width = W * dpr; this.canvas.height = H * dpr;
    this.canvas.style.height = H + 'px';
    var c = this.canvas.getContext('2d'); c.setTransform(dpr, 0, 0, dpr, 0, 0);
    c.clearRect(0, 0, W, H);
    c.font = '12px ' + cssVar('--mp-font-mono', 'monospace');
    this.hits = [];
    this.drawRuler(c, W); this.drawLanes(c);
    var y = RULER_H + PAD + this.nLanes * LANE_H + PAD;
    if (this.showNt && this.ppb >= NT_BAR) { this.drawNt(c, y); y += NT_H; }
    this.aaRows.forEach(function (f) { this.drawAa(c, f, y); y += AA_H; }, this);
  };
  Viewer.prototype.drawRuler = function (c, W) {
    var step = niceStep(90 / this.ppb), vs = this.viewStart, ve = vs + this.viewLen();
    c.fillStyle = cssVar('--mp-text-muted', '#6a6a6a'); c.strokeStyle = cssVar('--mp-border', '#ccc');
    c.textAlign = 'center'; c.textBaseline = 'top';
    for (var lin = Math.ceil(vs / step) * step; lin <= ve; lin += step) {
      var b = this.baseAt(lin); if (!b) continue;
      var x = this.x(lin);
      c.beginPath(); c.moveTo(x, RULER_H - 6); c.lineTo(x, RULER_H); c.stroke();
      c.fillText(String(b.pos), x, 2);
    }
    if (this.topology === 'circular') {
      [0, 1].forEach(function (k) {
        var lin = 1 + k * this.len;
        if (lin >= vs && lin <= ve) {
          var x = this.x(lin); c.save(); c.strokeStyle = cssVar('--mp-primary', '#337ab7'); c.setLineDash([3, 3]);
          c.beginPath(); c.moveTo(x, 0); c.lineTo(x, this.height()); c.stroke(); c.restore();
        }
      }, this);
    }
  };
  Viewer.prototype.drawLanes = function (c) {
    c.textAlign = 'center'; c.textBaseline = 'middle';
    this.feats.forEach(function (f) {
      var col = typeColor(f.type), y = this.laneY(f.lane), h = LANE_H - 6;
      this.segments(f).forEach(function (seg) {
        var x0 = Math.max(GUTTER, this.x(seg[0])), x1 = Math.min(this.wrap.clientWidth, this.x(seg[1] + 1));
        if (x1 - x0 < 1) return;
        var fwd = f.dir !== '-', head = Math.min(8, x1 - x0);
        c.beginPath();
        if (fwd) { c.moveTo(x0, y); c.lineTo(x1 - head, y); c.lineTo(x1, y + h / 2); c.lineTo(x1 - head, y + h); c.lineTo(x0, y + h); }
        else { c.moveTo(x1, y); c.lineTo(x0 + head, y); c.lineTo(x0, y + h / 2); c.lineTo(x0 + head, y + h); c.lineTo(x1, y + h); }
        c.closePath();
        c.fillStyle = col + '55'; c.fill();
        c.lineWidth = f.row === this.selected ? 2 : 1;
        c.strokeStyle = f.row === this.selected ? cssVar('--mp-primary', '#337ab7') : col;
        c.setLineDash((f.partial5 && fwd === true && seg[0] === f.pos1 + seg[2] * this.len) || (f.partial3 && !fwd) ? [3, 2] : []);
        c.stroke(); c.setLineDash([]);
        if (x1 - x0 > c.measureText(f.gene).width + 8) { c.fillStyle = cssVar('--mp-text', '#333'); c.fillText(f.gene, (x0 + x1) / 2, y + h / 2); }
        this.hits.push({ x0: x0, x1: x1, y0: y, y1: y + h, f: f });
      }, this);
    }, this);
  };
  Viewer.prototype.drawNt = function (c, y) {
    var vs = Math.floor(this.viewStart), ve = Math.ceil(this.viewStart + this.viewLen());
    c.textAlign = 'center'; c.textBaseline = 'middle';
    for (var lin = vs; lin <= ve; lin++) {
      var b = this.baseAt(lin); if (!b) continue;
      var x = this.x(lin), col = BASE[b.base] || BASE.N;
      c.fillStyle = col + (this.ppb >= NT_LETTER ? '99' : 'cc');
      c.fillRect(x, y + 2, Math.max(1, this.ppb - (this.ppb >= NT_LETTER ? 1 : 0)), NT_H - 4);
      if (this.ppb >= NT_LETTER) { c.fillStyle = '#ffffff'; c.fillText(b.base, x + this.ppb / 2, y + NT_H / 2); }
    }
    c.fillStyle = cssVar('--mp-text-muted', '#6a6a6a'); c.textAlign = 'right';
    c.fillText('nt', GUTTER - 6, y + NT_H / 2);
  };
  Viewer.prototype.drawAa = function (c, f, y) {
    var n = G.nCodons(f, this.len), vs = this.viewStart, ve = vs + this.viewLen();
    c.textAlign = 'right'; c.textBaseline = 'middle'; c.fillStyle = cssVar('--mp-text-muted', '#6a6a6a');
    c.fillText(f.gene, GUTTER - 6, y + AA_H / 2);
    c.textAlign = 'center';
    var ks = this.topology === 'circular' ? [-1, 0, 1] : [0];
    for (var i = 0; i < n; i++) {
      var letter = G.stopLetter(f, i, this.len); if (!letter) continue;
      var centre = G.codonCentre(f, i, this.len, this.topology);
      ks.forEach(function (k) {
        var lin = centre + k * this.len;
        if (lin < vs - 1 || lin > ve + 1) return;
        var x = this.x(lin) + this.ppb / 2;
        c.fillStyle = cssVar('--mp-surface-alt', '#f5f5f5');
        c.fillRect(x - 1.5 * this.ppb + 1, y + 2, 3 * this.ppb - 2, AA_H - 4);
        c.fillStyle = letter === '*' ? cssVar('--mp-danger', '#b02a37') : cssVar('--mp-text', '#333');
        c.fillText(letter, x, y + AA_H / 2);
        this.hits.push({ x0: x - 1.5 * this.ppb, x1: x + 1.5 * this.ppb, y0: y, y1: y + AA_H, f: f, codon: i, letter: letter });
      }, this);
    }
  };

  // ---- interaction ----
  Viewer.prototype.hitTest = function (px, py) {
    for (var i = this.hits.length - 1; i >= 0; i--) {
      var h = this.hits[i];
      if (px >= h.x0 && px <= h.x1 && py >= h.y0 && py <= h.y1) return h;
    }
    return null;
  };
  Viewer.prototype.click = function (px, py) {
    var h = this.hitTest(px, py);
    this.selected = h ? h.f.row : null;
    if (h && this.input && window.Shiny) {
      window.Shiny.setInputValue(this.input, { row: h.f.row, nonce: Date.now() }, { priority: 'event' });
    }
    this.draw();
  };
  Viewer.prototype.bindControls = function () {
    var self = this, sec = this.section || document, prefix = this.id.replace(/-canvas$/, '');
    var cv = this.canvas, dragging = null;
    cv.addEventListener('wheel', function (e) {
      e.preventDefault();
      var rect = cv.getBoundingClientRect(), px = e.clientX - rect.left;
      if (e.shiftKey) { self.viewStart += e.deltaY / self.ppb; self.clamp(); self.draw(); return; }
      self.zoom(Math.pow(1.15, -e.deltaY / 100), self.viewStart + (px - GUTTER) / self.ppb);
    }, { passive: false });
    cv.addEventListener('mousedown', function (e) { dragging = { x: e.clientX, start: self.viewStart, moved: false }; });
    window.addEventListener('mousemove', function (e) {
      if (!dragging) return;
      var dx = e.clientX - dragging.x; if (Math.abs(dx) > 2) dragging.moved = true;
      self.viewStart = dragging.start - dx / self.ppb; self.clamp(); self.draw();
    });
    window.addEventListener('mouseup', function (e) {
      if (!dragging) return;
      var rect = cv.getBoundingClientRect();
      if (!dragging.moved) self.click(e.clientX - rect.left, e.clientY - rect.top);
      dragging = null;
    });
    cv.addEventListener('mousemove', function (e) {
      var rect = cv.getBoundingClientRect(), px = e.clientX - rect.left, py = e.clientY - rect.top;
      var h = self.hitTest(px, py), b = self.baseAt(self.viewStart + (px - GUTTER) / self.ppb);
      if (!self.tip) return;
      if (!b) { self.tip.hidden = true; return; }
      var t = 'Position ' + b.pos.toLocaleString() + ', ' + b.base;
      if (h) t += ' | ' + h.f.gene + (h.codon !== undefined ? ' codon ' + (h.codon + 1) + ' ' + h.letter : ' (' + h.f.type + ')');
      self.tip.textContent = t; self.tip.hidden = false;
      self.tip.style.left = (px + 12) + 'px'; self.tip.style.top = (py + 12) + 'px';
    });
    cv.addEventListener('mouseleave', function () { if (self.tip) self.tip.hidden = true; });
    sec.querySelectorAll('[data-mpseq]').forEach(function (btn) {
      btn.addEventListener('click', function () {
        var a = btn.getAttribute('data-mpseq');
        if (a === 'zoom_in') self.zoom(2); else if (a === 'zoom_out') self.zoom(0.5);
        else if (a === 'whole') self.whole(); else if (a === 'fit' && self.selected !== null) self.fit(self.selected);
      });
    });
    var nt = document.getElementById(prefix + '-show_nt'), aa = document.getElementById(prefix + '-show_aa'), go = document.getElementById(prefix + '-goto');
    if (nt) nt.addEventListener('change', function () { self.showNt = nt.checked; self.draw(); });
    if (aa) aa.addEventListener('change', function () { self.showAa = aa.checked; self.draw(); });
    if (go) go.addEventListener('keydown', function (e) { if (e.key === 'Enter') { var v = parseInt(go.value, 10); if (v >= 1 && v <= self.len) self.goto(v); } });
  };

  function get(id) { return viewers[id] || (document.getElementById(id) ? (viewers[id] = new Viewer(id)) : null); }
  if (window.Shiny) {
    window.Shiny.addCustomMessageHandler('mpseq', function (p) { var v = get(p.id); if (v) v.load(p); });
    window.Shiny.addCustomMessageHandler('mpseq_select', function (p) { var v = get(p.id); if (v) v.fit(p.row); });
  }
  window.mpseq.state = function (id) {
    var v = viewers[id]; if (!v) return null;
    return { len: v.len, version: v.version, topology: v.topology, viewStart: v.viewStart, ppb: v.ppb, selected: v.selected,
             nLanes: v.nLanes, features: v.feats.map(function (f) { return { row: f.row, gene: f.gene, pos1: f.pos1, pos2: f.pos2, lane: f.lane }; }) };
  };
  window.mpseq.zoom = function (id, f) { var v = get(id); if (v) v.zoom(f); };
  window.mpseq.fit = function (id, row) { var v = get(id); if (v) v.fit(row); };
  window.mpseq.whole = function (id) { var v = get(id); if (v) v.whole(); };
  window.mpseq.goto = function (id, pos) { var v = get(id); if (v) v.goto(pos); };
  window.mpseq.laneY = function (id, lane) { var v = get(id); return v ? v.laneY(lane) + 8 : 0; };
  window.mpseq.hitTest = function (id, px, py) { var v = get(id); return v ? v.hitTest(px, py) : null; };
  window.mpseq.click = function (id, px, py) { var v = get(id); if (v) v.click(px, py); };
})();
```

Note: in the static test page the document may finish parsing before `Shiny` exists in a real app; in the app `custom.js` and `seqviewer.js` are loaded in `<head>` after Shiny, so `window.Shiny` is defined when the handlers register. The `mpseq_select` handler calls `fit`, which opens a closed `<details>`, as the spec's section 7 requires.

- [ ] **Step 4: Append the CSS**

Add to the `:root` block in `inst/app/www/custom.css` (after the existing tokens):

```css
  /* Feature types, shared by the annotation table badges (literal copies
     today) and the sequence viewer (read at draw time). */
  --mp-type-pcg:  #60BD68;
  --mp-type-trna: #F17CB0;
  --mp-type-rrna: #5DA5DA;
  --mp-type-ctrl: #FAA34A;
  --mp-type-orf:  #4D4D4D;
```

Append at the end of the file:

```css
/* ------------------------------------------------------------------ */
/* Sequence viewer (R/app_annotate_seqview.R, inst/app/www/seqviewer.js). */
/* ------------------------------------------------------------------ */
.mp-seqview-controls {
  display: flex;
  flex-wrap: wrap;
  align-items: flex-end;
  gap: var(--mp-space-2);
  margin: var(--mp-space-2) 0;
}
.mp-seqview-controls .form-group { margin-bottom: 0; }
.mp-seqview {
  position: relative;
  border: 1px solid var(--mp-border);
  border-radius: var(--mp-radius);
  background: var(--mp-surface);
  overflow: hidden;
}
.mp-seqview-canvas {
  display: block;
  width: 100%;
  cursor: grab;
}
.mp-seqview-canvas:active { cursor: grabbing; }
```

- [ ] **Step 5: Run the JavaScript tests to verify they pass**

Run: `CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-seqview-js.R")'`
Expected: PASS, 5 tests. If `expect_equal(round(s$wholePpb * 2000), 1000)` fails by the gutter width (the drawable width is the wrapper width minus 60 px), change the expectation to `round(s$wholePpb * 2000)` equal to `940`, since the page wrapper is 1000 px wide; keep the test honest to the implementation.

- [ ] **Step 6: Commit**

```bash
git add inst/app/www/seqviewer.js inst/app/www/custom.css tests/testthat/test-seqview-js.R
git commit -m "feat(seqview): canvas viewer with lanes, letters, zoom, hover, and clicks"
```

---

### Task 4: Shiny module, mount in the details window, docs, in-app check

**Files:**
- Modify: `R/app_annotate_seqview.R` (append `seqview_ui()`, `seqview_server()`)
- Modify: `R/app_annotate_details.R` (`annotate_details_modal()` around line 5290; `annotations_details_server()` after the `selected` reactive around line 722)
- Modify: `NEWS.md` (1.5.5 "New Features"), `vignettes/Test-Project-Annotate.Rmd` (section "Inspect a sample")
- Create: `dev/ui_review/steps/seqview_open.R` (harness, gitignored)

**Interfaces:**
- Consumes `seqview_payload()` (Task 1), the JavaScript handlers `mpseq` / `mpseq_select` and the input `<ns>-pick` (Task 3), `get_assembly(ID, path, scaffold, con)` (R/app_assemble_utils.R:785), `gargoyle::watch("annotations_modal")`, the details server's `asmb_edit_tick` reactiveVal (line 2937) and `rv` (with `rv$annotations`, `rv$updating$ID/path/scaffold/topology`, `rv$editing$assembly`).
- Produces `seqview_ui(id)` (a `tags$details` section) and `seqview_server(id, rv, tick, selected)` returning `list(pick = reactive)`.

- [ ] **Step 1: Append the module to R/app_annotate_seqview.R**

```r
#' Sequence viewer section (tools/nt_viewer_spec.md, section 3)
#' @noRd
seqview_ui <- function(id) {
  ns <- NS(id)
  btn <- function(action, label, icon = NULL, title = NULL) {
    tags$button(
      type = "button", class = "btn btn-default", `data-mpseq` = action,
      title = title, `aria-label` = title %||% label, icon, label
    )
  }
  tags$details(
    id = ns("section"),
    tags$summary("Sequence"),
    div(
      class = "mp-seqview-controls",
      mp_checkbox(ns("show_nt"), label = "Nucleotides", value = TRUE),
      mp_checkbox(ns("show_aa"), label = "Amino acids", value = TRUE),
      numericInput(ns("goto"), "Position:", value = NA, min = 1, step = 1, width = "130px"),
      btn("fit", "Fit gene"),
      btn("whole", "Whole genome"),
      btn("zoom_in", NULL, icon("magnifying-glass-plus"), "Zoom in"),
      btn("zoom_out", NULL, icon("magnifying-glass-minus"), "Zoom out")
    ),
    div(class = "mp-coverage-caption",
        "Drag to pan, scroll or pinch to zoom; click a gene to select its row. Letters appear when zoomed in."),
    uiOutput(ns("empty")),
    div(
      class = "mp-seqview",
      tags$canvas(id = ns("canvas"), class = "mp-seqview-canvas"),
      tags$div(id = ns("tip"), class = "mp-maptoref-tip", hidden = NA)
    )
  )
}

#' @param rv the annotate module's reactive values (annotations, updating, editing)
#' @param tick reactiveVal bumped when the assembly sequence changes
#' @param selected reactive of the annotation table's selected row indices
#' @noRd
seqview_server <- function(id, rv, tick, selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The unit sequence: the edit session's copy while one is open, else the
    # stored assembly. Re-read when the window (re)opens or the sequence is
    # rewritten (trim, linearize, restore); shiny drops the downstream
    # invalidation when the string is unchanged.
    unit_seq <- reactive({
      tick()
      gargoyle::watch("annotations_modal")
      s <- rv$editing$assembly
      if (is.null(s)) {
        s <- tryCatch(
          get_assembly(rv$updating$ID, rv$updating$path, rv$updating$scaffold,
                       session$userData$con),
          error = function(e) NULL
        )
      }
      if (is.null(s) || length(s) == 0L) return(NULL)
      as.character(s[[1]])
    })
    version <- reactiveVal(0L)
    observeEvent(unit_seq(), version(isolate(version()) + 1L), ignoreNULL = FALSE)

    observe({
      req(rv$annotations)
      s <- unit_seq()
      if (is.null(s)) {
        output$empty <- renderUI(div(class = "mp-coverage-caption",
                                     "No sequence stored for this assembly."))
        return()
      }
      output$empty <- renderUI(NULL)
      sel <- selected()
      p <- seqview_payload(
        rv$annotations, s, rv$updating$topology %||% "linear",
        paste(rv$updating$ID, rv$updating$path, rv$updating$scaffold, sep = "."),
        selected = if (length(sel) == 1L) sel else NULL, version = version()
      )
      p$id <- ns("canvas")
      p$input <- ns("pick")
      session$sendCustomMessage("mpseq", p)
    })

    observeEvent(selected(), {
      sel <- selected()
      if (length(sel) == 1L) {
        session$sendCustomMessage("mpseq_select", list(id = ns("canvas"), row = sel))
      }
    })

    list(pick = reactive(input$pick))
  })
}
```

- [ ] **Step 2: Mount it in the details window**

In `annotate_details_modal()` (R/app_annotate_details.R), directly before the line
`tags$hr(style = "margin: 4px 0; border: none; border-top: 1px solid #e0e0e0;"),`
that precedes `tags$details(tags$summary("Coverage Map"), ...)` (about line 5290), insert:

```r
    seqview_ui(ns("seqview")),
```

In `annotations_details_server()`, after the `selected <- reactive({ ... })` block (it starts about line 722; find its closing `})`), insert:

```r
    # Sequence viewer under the table: follows every edit through rv$annotations
    # and the assembly tick; a click on a gene arrow selects its table row.
    sv <- seqview_server(
      "seqview", rv, asmb_edit_tick,
      reactive(reactable::getReactableState("table", "selected"))
    )
    observeEvent(sv$pick(), {
      reactable::updateReactable("table", selected = as.integer(sv$pick()$row))
    })
```

`asmb_edit_tick` is defined at about line 2937, later in the file than this insertion; R closures resolve it at call time inside the module server, so the order does not matter, but keep the insertion after the `selected` reactive so the table exists.

- [ ] **Step 3: Parse, load, and run the quick tests**

Run: `Rscript -e 'devtools::load_all("."); invisible(annotate_details_modal); testthat::test_file("tests/testthat/test-variant-parity.R"); testthat::test_file("tests/testthat/test-no-duplicate-definitions.R"); testthat::test_file("tests/testthat/test-seqview-payload.R")'`
Expected: all PASS; no parse error.

- [ ] **Step 4: Write the harness script and run it**

```r
# dev/ui_review/steps/seqview_open.R
nav(wait = 8); wait_for(ROWS_ASM); Sys.sleep(2)
goto("Annotate", wait = 8); wait_for(ROWS_ANN); Sys.sleep(3)
click_in_row("SRR19434536", 2, rows = ROWS_ANN); Sys.sleep(12)
cat("details open:", js("!!document.querySelector('.modal-content')"), "\n")
cat("section closed at open:", js("!document.getElementById('annotate-annotations-seqview-section').open"), "\n")
st <- function() js("JSON.stringify(window.mpseq.state('annotate-annotations-seqview-canvas'))")
cat("state after load:", substr(st(), 1, 200), "\n")
# click the nad1 row (6th row in the fixture) in the annotation table
js("(function(){var rs=[...document.querySelectorAll('#annotate-annotations-table .rt-tbody .rt-tr-group')]; var r=rs.find(x=>x.innerText.includes('nad1')); r.querySelector('.rt-td').click(); return 'ok';})()"); Sys.sleep(4)
cat("section open after row click:", js("document.getElementById('annotate-annotations-seqview-section').open"), "\n")
cat("state after fit:", substr(st(), 1, 200), "\n")
js("document.querySelector('.modal').scrollTop = document.getElementById('annotate-annotations-seqview-section').offsetTop - 60"); Sys.sleep(1)
shot("seqview-fit-nad1")
js("window.mpseq.whole('annotate-annotations-seqview-canvas')"); Sys.sleep(1); shot("seqview-whole")
js("window.mpseq.goto('annotate-annotations-seqview-canvas', 16490)"); Sys.sleep(1); shot("seqview-origin")
cat("log ok\n")
b$close()
```

Run:
```bash
dev/ui_review/app.sh start ~/MitoPilot_scratch/new_test_project 3856
cd dev/ui_review && mkdir -p shots/seqview && APP_PORT=3856 SHOT_DIR=$PWD/shots/seqview CHROMOTE_CHROME=$PWD/chrome-wrap.sh Rscript capture_lib.R steps/seqview_open.R
grep -h "Warning\|Error" app_logs/app_3856.log | sort | uniq -c
```
Expected: "section closed at open: TRUE"; after the row click "section open after row click: TRUE", state `selected` = the nad1 row and `ppb` > 1; three screenshots; no Warning/Error in the log. Look at the three screenshots: ruler numbers readable, arrows in lanes with names, nucleotide letters at fit zoom, an amino-acid row labelled "nad1" with letters centred on codons, the dashed origin rule visible in `seqview-origin`. Fix what is wrong in Task 3's code and re-run before committing. Stop the app: `dev/ui_review/app.sh stop 3856`.

If the module id differs from `annotate-annotations-seqview-...` (check with `js("[...document.querySelectorAll('canvas')].map(c=>c.id)")`), adjust the ids in the script, not the code.

- [ ] **Step 5: Docs**

In `NEWS.md` under `# MitoPilot 1.5.5` / `## New Features`, add a subsection after the MapToRef one:

```markdown
### Sequence viewer in the annotation window

- The annotation details window has a **Sequence** section: the assembly's nucleotides with the annotated genes drawn in lanes above them and, for each protein-coding gene, its translated amino acids lined up under their codons. Click a gene in the table to jump to it; drag, scroll, or use the zoom buttons to move around; a circular assembly wraps through its origin. Every edit in the window (deleting, merging, moving a start or stop codon, linearizing, trimming) is reflected immediately.
```

In `vignettes/Test-Project-Annotate.Rmd`, at the end of the "Inspect a sample" section (before "## Manually fix annotations"), add:

```markdown
Below the annotation table, the **Sequence** section shows the assembly itself:
genes as arrows in lanes, the nucleotides once you zoom in, and the translated
amino acids of each protein-coding gene under their codons. Click a row in the
table to jump to that gene, or use Whole genome to step back out. The view
follows every edit you make in this window.
```

- [ ] **Step 6: Commit**

```bash
git add R/app_annotate_seqview.R R/app_annotate_details.R NEWS.md vignettes/Test-Project-Annotate.Rmd
git commit -m "feat(seqview): Sequence section in the annotation details window"
```

---

### Task 5: Edit tracking check (nudge, delete, restore, linearize, trim)

**Files:**
- Create: `dev/ui_review/steps/seqview_edits.R` (harness, gitignored)
- Modify (only if a check fails): `R/app_annotate_seqview.R`, `inst/app/www/seqviewer.js`

**Interfaces:**
- Consumes everything from Task 4. Produces no new interface; this task proves section 8 of the spec.

- [ ] **Step 1: Prepare an editable sample on the scratch copy**

```bash
sqlite3 ~/MitoPilot_scratch/new_test_project/.sqlite "UPDATE annotate SET annotate_lock = 0 WHERE ID = 'SRR19434536'"
dev/ui_review/app.sh start ~/MitoPilot_scratch/new_test_project 3856
```

- [ ] **Step 2: Write the harness script**

```r
# dev/ui_review/steps/seqview_edits.R
CV <- "annotate-annotations-seqview-canvas"
st <- function() jsonlite::fromJSON(js(sprintf("JSON.stringify(window.mpseq.state('%s'))", CV)))
feat <- function(gene) { s <- st(); s$features[s$features$gene == gene, ] }
nav(wait = 8); wait_for(ROWS_ASM); Sys.sleep(2)
goto("Annotate", wait = 8); wait_for(ROWS_ANN); Sys.sleep(3)
click_in_row("SRR19434536", 2, rows = ROWS_ANN); Sys.sleep(12)
v0 <- st()$version
# 1. select nad1, enter edit mode, nudge the start codon one codon later, expect pos1 + 3 in the viewer
js("(function(){var rs=[...document.querySelectorAll('#annotate-annotations-table .rt-tbody .rt-tr-group')]; rs.find(x=>x.innerText.includes('nad1')).querySelector('.rt-td').click(); return 'ok';})()"); Sys.sleep(3)
p_before <- feat("nad1")$pos1
click_contains("Edit", "button"); Sys.sleep(4)
cat("edit buttons:", modal_buttons(), "\n")   # find the exact label of the start +3 nudge button here
click("#annotate-annotations-start-add", wait = 4)   # adjust to the real id printed above
cat("nad1 pos1 before/after nudge:", p_before, feat("nad1")$pos1, "\n")
click_contains("Discard", "button"); Sys.sleep(3)
cat("nad1 pos1 after discard:", feat("nad1")$pos1, "\n")
# 2. delete trnF, expect it gone; restore, expect it back
js("(function(){var rs=[...document.querySelectorAll('#annotate-annotations-table .rt-tbody .rt-tr-group')]; rs.find(x=>x.innerText.includes('trnF')).querySelector('.rt-td').click(); return 'ok';})()"); Sys.sleep(3)
click_contains("Delete", "button"); Sys.sleep(3); swal_accept(); Sys.sleep(4)
cat("trnF rows after delete:", nrow(feat("trnF")), "\n")
click_contains("Restore", "button"); Sys.sleep(3); swal_accept(); Sys.sleep(4)
cat("trnF rows after restore:", nrow(feat("trnF")), "\n")
# 3. linearize before trnF, expect version bump and same length
js("(function(){var rs=[...document.querySelectorAll('#annotate-annotations-table .rt-tbody .rt-tr-group')]; rs.find(x=>x.innerText.includes('cox1')).querySelector('.rt-td').click(); return 'ok';})()"); Sys.sleep(3)
click_contains("Linearize", "button"); Sys.sleep(3); cat("swal:", substr(swal_text(160), 1, 120), "\n"); swal_accept(); Sys.sleep(12)
s <- st(); cat("after linearize: version", v0, "->", s$version, " topology", s$topology, " len", s$len, "\n")
shot("seqview-after-linearize")
b$close()
```

Run it as in Task 4 (port 3856, `SHOT_DIR=$PWD/shots/seqview`). Read `modal_buttons()` output on the first run to learn the real ids/labels of the nudge, Delete, Restore, and Linearize controls, then fix the script's selectors (not the code) and re-run.

- [ ] **Step 3: Interpret and fix**

Expected: nudge moves `pos1` by 3 and Discard restores it; delete removes the trnF row from the viewer state and Restore brings it back; linearize bumps `version`, `topology` reads "linear", `len` is unchanged, and the screenshot shows the origin rule gone. Any miss is a defect in the observer dependencies of `seqview_server()` (Task 4) or in `Viewer.prototype.load()` (Task 3): fix there, re-run the JavaScript tests and this script.

- [ ] **Step 4: Restore the scratch copy and commit**

```bash
dev/ui_review/app.sh stop 3856
sqlite3 ~/MitoPilot_scratch/new_test_project/.sqlite "UPDATE annotate SET annotate_lock = 1 WHERE ID = 'SRR19434536'"
git add -A R/ inst/app/www/seqviewer.js   # only if fixes were needed
git commit -m "fix(seqview): follow edits through the assembly tick"   # only if fixes were needed
```

If nothing needed fixing, there is nothing to commit; note the passing run in the handoff.

---

### Task 6: Reverse-complement the "nt" copy for minus-strand genes

**Files:**
- Modify: `R/annotate_utils.R` (add `feature_nt()` after `extract_circ_region()`, about line 244)
- Modify: `R/app_annotate_details.R` (the `copy_fas` handler, about lines 840-859)
- Test: `tests/testthat/test-feature-nt.R`

**Interfaces:**
- Produces `feature_nt(seq, pos1, pos2, direction)` returning a `Biostrings::DNAString` of the feature in reading direction (reverse complement for `"-"`), wrap-around aware through `extract_circ_region()`.

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-feature-nt.R
test_that("feature_nt reads minus-strand and wrapped features in gene direction", {
  s <- Biostrings::DNAString("AAACCCGGGTTT")
  expect_equal(as.character(feature_nt(s, 4, 6, "+")), "CCC")
  expect_equal(as.character(feature_nt(s, 4, 9, "-")), "CCCGGG")
  expect_equal(as.character(feature_nt(s, 7, 12, "-")), "AAACCC")
  expect_equal(as.character(feature_nt(s, 11, 2, "+")), "TTAA")
  expect_equal(as.character(feature_nt(s, 11, 2, "-")), "TTAA")
})
```

- [ ] **Step 2: Run it to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-feature-nt.R")'`
Expected: FAIL, `could not find function "feature_nt"`.

- [ ] **Step 3: Implement and use it**

```r
# R/annotate_utils.R, after extract_circ_region()
#' Feature sequence in reading direction (reverse complement on "-")
#' @noRd
feature_nt <- function(seq, pos1, pos2, direction) {
  x <- extract_circ_region(seq, pos1, pos2)
  if (identical(direction, "-")) x <- Biostrings::reverseComplement(x)
  x
}
```

In the `copy_fas` handler in `R/app_annotate_details.R`, replace the `extract_circ_region(seq, pos1, pos2)` call that produces the copied sequence with `feature_nt(seq, pos1, pos2, rv$annotations$direction[idx])` (keep the surrounding header and clipboard code unchanged).

- [ ] **Step 4: Run the tests**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-feature-nt.R"); testthat::test_file("tests/testthat/test-no-duplicate-definitions.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/annotate_utils.R R/app_annotate_details.R tests/testthat/test-feature-nt.R
git commit -m "fix(annotate): nt copy of a minus-strand gene is its reverse complement"
```

---

### Task 7: Full suite and handoff

**Files:**
- Create: `dev/HANDOFF_MitoPilot_seq-viewer_<date>.md`

- [ ] **Step 1: Run the full suite**

Run: `Rscript -e 'devtools::load_all("."); devtools::test(reporter = "summary")' > dev/ui_review/test_seqview.log 2>&1; grep -A 3 "══ Failed" dev/ui_review/test_seqview.log | head; grep -c "blastn not installed" dev/ui_review/test_seqview.log`
Expected: exactly one failure (pre-existing, `test-ambiguous-cds-translation.R:53`), 23 skips, the four pre-existing export-topology warnings, and the seqview tests passing (or the JavaScript ones skipped if chromote is unavailable, which must be stated).

- [ ] **Step 2: Write the handoff**

Contents: branch and commits (`git log --oneline map-to-ref-assembly..seq-viewer`), what works (with the screenshot paths under `dev/ui_review/shots/seqview/`), what the harness exercised, known limits (no keyboard operation of the canvas; amino-acid rows hidden below 4 px per base; the table badge colours are still literals in the table's JS cell), and how to run the JavaScript tests (`CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh`).

- [ ] **Step 3: Commit the handoff? No.** `dev/` is untracked scratch; leave it on disk and say where it is.

---

## Self-review against the spec

- Section 3 (placement, controls, caption, closed by default, opens on row click): Task 4 UI + `fit()` opening the details in Task 3.
- Section 4 (payload fields): Task 1; `id` and `input` added in Task 4.
- Section 5 (ruler, lanes with packing and wrap glyph, partial dashes, selected outline, nt thresholds 8 and 3, AA threshold 4, codon placement and "*" rule, gene gutter labels): Task 3 draw functions; the wrap "glyph" is the dashed origin rule crossing the lane, which the spec allows as the origin marker.
- Section 6 (wheel/pinch/drag/shift-wheel, buttons, Whole genome, Fit gene, Position, circular wrapping, hover tooltip, click selects row): Task 3 `bindControls`, `click`, `zoom`, `goto`, `whole`, `fit`; Task 4 forwards the pick to the table.
- Section 7 (table selection jumps): Task 4 `observeEvent(selected())`.
- Section 8 (edit tracking; keep viewport when only features changed): Task 3 `load()` compares `version` and `len`; Task 4 observer dependencies; Task 5 proves it.
- Section 9 (files): all present; the JavaScript tests live in `tests/testthat/seqview/`.
- Section 10 (no sequence, no features, short translation): Task 4 empty notice; Task 3 draws ruler only; `stopLetter` stops at the string end.
- Section 12 (copy fix): Task 6.
- Pinch zoom: browsers deliver pinch as a wheel event with `ctrlKey`; the wheel handler covers it.
- Type consistency: `seqview_payload` fields match the JavaScript reads (`pos1`, `pos2`, `dir`, `partial5`, `partial3`, `translation`, `row`, `gene`, `type`); `mpseq.state` shape matches both test files; module ids in the harness follow `annotate-annotations-seqview-*` and the scripts verify them before use.
