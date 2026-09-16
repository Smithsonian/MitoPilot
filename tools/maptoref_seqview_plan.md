# MapToRef Sequence Viewer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the MapToRef "Coverage Map" and "Read Pileup" ggplot panels in the assembly details window with the canvas sequence viewer, extended with a consensus row and an on-demand read-lane mode.

**Architecture:** `inst/app/www/seqviewer.js` gains an optional second sequence row (`seq2`, difference overlay) and a reads mode (browser asks R for reads through a Shiny input when the view is at most 1,000 bp wide; R replies with a custom message). `R/maptoref_viz_data.R` gains two pure helpers that build the load payload and the reads reply. `R/app_assemble_maptoref_viewer.R` is rewritten to send those and drops all ggplot code.

**Tech Stack:** R / Shiny, plain JS canvas, testthat, chromote (JS tests, harness).

**Spec:** `tools/maptoref_seqview_spec.md` (the annotate viewer's contract is `tools/nt_viewer_spec.md`).

## Global Constraints

- Branch `feat/maptoref-seqview`. Never push. Commits: brief conventional subject, no attribution lines, no session trailer.
- ASCII only in every touched file: `grep -nP '[^\x00-\x7F]' <file>` must print nothing.
- Minimal comments. No em dashes anywhere.
- R tests: `Rscript -e 'devtools::test(filter = "<name>")'`. Baseline for the full suite is `FAIL 1 | WARN 4` (the failure is `test-ambiguous-cds-translation.R:53`, pre-existing).
- JS tests need Chrome: `CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh Rscript -e 'devtools::test(filter = "seqview-js")'`. Without it they skip, which is not a pass.
- The annotate viewer's visible behaviour must not change: the annotate payload never sends `seq2`, `seqLabel`, `readsInput`, so every new branch in the JS must be gated on those fields.
- Read colours come from CSS custom properties `--mp-type-rrna` (forward) and `--mp-type-ctrl` (reverse), read with the existing `cssVar()`.
- Reads request window: 1-based inclusive, may be padded up to `readsMaxBp` wide; R clamps to `[1, len]`.

---

### Task 1: R payload and reply helpers

**Files:**
- Modify: `R/maptoref_viz_data.R` (append at end; also delete `maptoref_bin_depth`, lines 91-107, which only the ggplot code used)
- Test: `tests/testthat/test-maptoref-viz-data.R` (delete the three `maptoref_bin_depth` tests at lines 100-118; append new tests)

**Interfaces:**
- Produces: `MTR_READS_MAX_BP <- 1000L`
- Produces: `maptoref_seqview_payload(depth, features, ref_seq, cons_seq, topology, unit, version)` returning a list with `unit, len, topology, seq, seqLabel, seq2Label, version, features, readsMaxBp` and optionally `seq2`, `depth`. No `id`, `input`, `readsInput` (the module adds those).
- Produces: `maptoref_reads_reply(w, start, end, nonce)` where `w` is a `maptoref_window_reads()` result; returns a list `nonce, start, end, reads, mm, del, ins, nShown, nTotal` with each frame as a list of row-lists (Shiny's JSON serialiser is column-oriented for data frames, so rows must be built here).

- [ ] **Step 1: Delete the `maptoref_bin_depth` tests and write the failing tests**

Remove the three `test_that("maptoref_bin_depth ...` blocks from `tests/testthat/test-maptoref-viz-data.R`. Append at the end of the file:

```r
test_that("maptoref_seqview_payload maps features and indexes depth by position", {
  depth <- data.frame(Position = c(1L, 2L, 4L), Depth = c(5, 0, 9))
  feats <- data.frame(
    type = c("CDS", "tRNA", "D-loop", "misc_feature"),
    gene = c("ND1", "trnQ", "D-loop", "x"),
    start = c(1L, 2L, 3L, 4L), end = c(2L, 3L, 4L, 4L),
    strand = c("+", "-", "+", "+"), stringsAsFactors = FALSE
  )
  p <- maptoref_seqview_payload(depth, feats, "acgt", "ACGA", "circular", "S1", 3L)
  expect_equal(p$len, 4L)
  expect_equal(p$seq, "ACGT")
  expect_equal(p$seq2, "ACGA")
  expect_equal(p$topology, "circular")
  expect_equal(p$version, 3L)
  expect_equal(p$seqLabel, "Reference")
  expect_equal(p$seq2Label, "Consensus")
  expect_equal(p$readsMaxBp, 1000L)
  expect_equal(vapply(p$features, function(f) f$type, ""), c("PCG", "tRNA", "CTRL", "misc_feature"))
  expect_equal(vapply(p$features, function(f) f$row, 1L), 1:4)
  expect_equal(p$features[[2]]$pos1, 2L)
  expect_equal(p$features[[2]]$pos2, 3L)
  expect_equal(p$features[[2]]$dir, "-")
  expect_false(p$features[[1]]$partial5)
  expect_equal(p$features[[1]]$notes, "")
  expect_equal(p$depth, c(5L, 0L, NA, 9L))
  expect_null(p$input)
  expect_null(p$readsInput)
})

test_that("maptoref_seqview_payload drops a consensus whose length differs", {
  depth <- data.frame(Position = 1:4, Depth = rep(1, 4))
  feats <- maptoref_read_features(file.path(tempdir(), "nope.csv"))
  p <- maptoref_seqview_payload(depth, feats, "ACGT", "ACG", "linear", "S1", 1L)
  expect_null(p$seq2)
  expect_length(p$features, 0L)
  p2 <- maptoref_seqview_payload(depth, feats, "ACGT", NA_character_, "linear", "S1", 1L)
  expect_null(p2$seq2)
  p3 <- maptoref_seqview_payload(depth, feats, "ACGT", "ACGT", NA_character_, "S1", 1L)
  expect_equal(p3$topology, "linear")
})

test_that("maptoref_seqview_payload falls back to the depth length without a reference", {
  depth <- data.frame(Position = 1:4, Depth = rep(1, 4))
  feats <- maptoref_read_features(file.path(tempdir(), "nope.csv"))
  p <- maptoref_seqview_payload(depth, feats, NA_character_, NA_character_, "linear", "S1", 1L)
  expect_equal(p$len, 4L)
  expect_equal(p$seq, "")
  expect_equal(p$depth, rep(1L, 4))
})

test_that("maptoref_reads_reply turns the reader's frames into row lists", {
  bam <- mtr_viz_bam()
  w <- maptoref_window_reads(bam, 1L, 60L, mtr_viz_ref())
  r <- maptoref_reads_reply(w, 1L, 60L, 7)
  expect_equal(r$nonce, 7)
  expect_equal(r$start, 1L)
  expect_equal(r$end, 60L)
  expect_equal(r$nShown, 5L)
  expect_equal(r$nTotal, 5L)
  expect_length(r$reads, 5L)
  expect_setequal(names(r$reads[[1]]), c("row", "start", "end", "strand"))
  expect_type(r$reads[[1]]$strand, "character")
  expect_equal(r$mm[[1]]$pos, 25L)
  expect_equal(r$mm[[1]]$base, "T")
  expect_equal(r$del[[1]]$start, 35L)
  expect_equal(r$ins[[1]]$len, 2L)
})

test_that("maptoref_reads_reply sends empty arrays for an empty window", {
  w <- maptoref_window_reads(file.path(tempdir(), "nope.bam"), 1L, 60L, mtr_viz_ref())
  r <- maptoref_reads_reply(w, 1L, 60L, 1)
  expect_equal(r$reads, list())
  expect_equal(r$mm, list())
  expect_equal(r$nTotal, 0L)
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "maptoref-viz-data")'`
Expected: the five new tests fail with `could not find function "maptoref_seqview_payload"` / `"maptoref_reads_reply"`; the rest pass.

- [ ] **Step 3: Delete `maptoref_bin_depth` and append the helpers**

Delete the `maptoref_bin_depth` roxygen block and function from `R/maptoref_viz_data.R`. Append at the end of the file:

```r
#' Widest view, in bases, at which the browser asks for read lanes.
#' @noRd
MTR_READS_MAX_BP <- 1000L

#' Sequence viewer payload for a MapToRef sample (tools/maptoref_seqview_spec.md, 4.1)
#'
#' Pure. GenBank feature types are mapped onto the annotation table's colour
#' tokens where one exists; other types fall through to the viewer's grey.
#' The consensus is only sent when it lies base for base over the reference.
#' @noRd
maptoref_seqview_payload <- function(depth, features, ref_seq, cons_seq,
                                     topology, unit, version) {
  seq <- toupper(as.character(ref_seq)[1])
  if (is.na(seq)) seq <- ""
  len <- nchar(seq)
  if (len == 0L) len <- nrow(depth)
  type_map <- c(CDS = "PCG", "D-loop" = "CTRL")
  feats <- lapply(seq_len(nrow(features)), function(i) {
    ty <- as.character(features$type[i])
    list(
      row = i,
      type = if (ty %in% names(type_map)) unname(type_map[ty]) else ty,
      gene = as.character(features$gene[i]),
      pos1 = as.integer(features$start[i]),
      pos2 = as.integer(features$end[i]),
      dir = as.character(features$strand[i]),
      partial5 = FALSE, partial3 = FALSE, notes = ""
    )
  })
  out <- list(
    unit = unit, len = as.integer(len),
    topology = if (identical(topology, "circular")) "circular" else "linear",
    seq = seq, seqLabel = "Reference", seq2Label = "Consensus",
    version = as.integer(version), features = feats,
    readsMaxBp = MTR_READS_MAX_BP
  )
  cons <- toupper(as.character(cons_seq)[1])
  if (!is.na(cons) && nchar(cons) == len && nchar(seq) > 0L) out$seq2 <- cons
  if (is.data.frame(depth) && nrow(depth) > 0L && len > 0L) {
    out$depth <- as.integer(depth$Depth[match(seq_len(len), depth$Position)])
  }
  out
}

#' Reads reply for the sequence viewer (tools/maptoref_seqview_spec.md, 4.3)
#'
#' Pure. Frames become lists of rows because Shiny serialises a data.frame
#' column-wise. Read names are not sent.
#' @noRd
maptoref_reads_reply <- function(w, start, end, nonce) {
  rows <- function(df, cols) {
    df <- df[, cols, drop = FALSE]
    lapply(seq_len(nrow(df)), function(i) {
      lapply(as.list(df[i, , drop = FALSE]), function(v) {
        if (is.factor(v)) as.character(v) else v
      })
    })
  }
  list(
    nonce = nonce, start = as.integer(start), end = as.integer(end),
    reads = rows(w$reads, c("row", "start", "end", "strand")),
    mm = rows(w$mm, c("row", "pos", "base")),
    del = rows(w$del, c("row", "start", "end")),
    ins = rows(w$ins, c("row", "pos", "len")),
    nShown = as.integer(w$n_shown), nTotal = as.integer(w$n_total)
  )
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "maptoref-viz-data")'`
Expected: all pass, 0 failures. (`maptoref_bin_depth` is still referenced by `R/app_assemble_maptoref_viewer.R`; that file is rewritten in Task 4. `devtools::test()` loads the package with `load_all`, which does not fail on a missing function until it is called, so this task's tests still pass.)

- [ ] **Step 5: ASCII check and commit**

```bash
grep -nP '[^\x00-\x7F]' R/maptoref_viz_data.R tests/testthat/test-maptoref-viz-data.R
git add R/maptoref_viz_data.R tests/testthat/test-maptoref-viz-data.R
git commit -m "feat(maptoref): sequence viewer payload and reads reply helpers"
```

---

### Task 2: JS consensus row, difference overlay, zero-depth marks

**Files:**
- Modify: `inst/app/www/seqviewer.js`
- Modify: `tests/testthat/seqview/index.html` (add a `sv-show_reads` checkbox; used in Task 3 too)
- Test: `tests/testthat/test-seqview-js.R`

**Interfaces:**
- Produces: `window.mpseq.geom.diffOverlay(a, b)` -> array of `"same" | "mismatch" | "n" | "gap"` per character of `b` compared with `a`.
- Produces: `window.mpseq.geom.readsCover(have, want)` -> boolean, `have` may be null; both are `{start, end}`.
- Produces: viewer fields `seq2`, `seqLabel`, `seq2Label`; `Viewer.prototype.ntOn()`; `drawNt(c, y, seq, label, ref)` (generalised from the old `drawNt(c, y)`); `this.section` now matches `details` or `.mp-maptoref`.
- Produces: `window.mpseq.state(id)` gains `seq2Len` (length of the stored consensus, 0 if none).

- [ ] **Step 1: Add the reads checkbox to the test page**

In `tests/testthat/seqview/index.html`, after the `sv-show_aa` checkbox line add:

```html
      <input type="checkbox" id="sv-show_reads" checked>
```

- [ ] **Step 2: Write the failing tests**

Append to `tests/testthat/test-seqview-js.R`:

```r
test_that("diffOverlay classes each consensus base and readsCover checks containment", {
  b <- sv_page()
  r <- js(b, "(function(){var g=window.mpseq.geom;
    return JSON.stringify({d: g.diffOverlay('ACGTA', 'AGGN-'),
      c1: g.readsCover(null, {start:1,end:5}), c2: g.readsCover({start:1,end:10}, {start:2,end:9}),
      c3: g.readsCover({start:1,end:10}, {start:2,end:11}), c4: g.readsCover({start:1,end:10}, {start:1,end:10})});})()")
  s <- jsonlite::fromJSON(r)
  expect_equal(s$d, c("same", "mismatch", "same", "n", "gap"))
  expect_false(s$c1); expect_true(s$c2); expect_false(s$c3); expect_true(s$c4)
})

test_that("a second sequence adds one row at letter zoom and none when nucleotides are off", {
  b <- sv_page()
  r <- js(b, "(function(){
    var seq = Array(2000).join('ACGT').slice(0, 2000), seq2 = seq.slice(0, 1000) + 'N' + seq.slice(1001);
    var base = {id:'sv-canvas', unit:'S1', len:2000, topology:'linear', seq:seq, version:1, selected:null, features:[]};
    window.__handlers.mpseq(base); window.mpseq.goto('sv-canvas', 1000);
    var h0 = window.mpseq.state('sv-canvas').height;
    window.__handlers.mpseq(Object.assign({seq2:seq2, seqLabel:'Reference', seq2Label:'Consensus'}, base));
    window.mpseq.goto('sv-canvas', 1000);
    var s1 = window.mpseq.state('sv-canvas'), h1 = s1.height;
    window.mpseq.whole('sv-canvas'); var h2 = window.mpseq.state('sv-canvas').height;
    window.mpseq.goto('sv-canvas', 1000);
    var box = document.getElementById('sv-show_nt'); box.checked = false; box.dispatchEvent(new Event('change'));
    var h3 = window.mpseq.state('sv-canvas').height;
    box.checked = true; box.dispatchEvent(new Event('change'));
    return JSON.stringify({h0:h0, h1:h1, h2:h2, h3:h3, seq2Len:s1.seq2Len});})()")
  s <- jsonlite::fromJSON(r)
  expect_equal(s$h1 - s$h0, 20)
  expect_equal(s$seq2Len, 2000)
  expect_equal(s$h2, s$h0 - 20)
  expect_equal(s$h3, s$h0 - 20)
})
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh Rscript -e 'devtools::test(filter = "seqview-js")'`
Expected: the two new tests fail (`diffOverlay` is not a function; `h1 - h0` is 0); the eleven existing tests pass.

- [ ] **Step 4: Implement in `inst/app/www/seqviewer.js`**

In the first IIFE, after `segOwns` and before `window.mpseq = ...`, add:

```js
  // Per-base class of a consensus string b laid over a reference string a.
  function diffOverlay(a, b) {
    var out = [];
    for (var i = 0; i < b.length; i++) {
      var y = b.charAt(i), x = a.charAt(i);
      out.push(y === 'N' ? 'n' : y === '-' ? 'gap' : y !== x ? 'mismatch' : 'same');
    }
    return out;
  }
  // Whether a reads reply covering `have` already covers the wanted window.
  function readsCover(have, want) {
    return !!have && have.start <= want.start && have.end >= want.end;
  }
```

and extend the exported object:

```js
  window.mpseq.geom = { span: span, wraps: wraps, lanes: lanes, nCodons: nCodons,
                        codonCentre: codonCentre, stopLetter: stopLetter,
                        partialEdge: partialEdge, segOwns: segOwns,
                        diffOverlay: diffOverlay, readsCover: readsCover };
```

In the second IIFE:

1. Constructor: change `this.section = this.canvas.closest('details');` to `this.section = this.canvas.closest('details, .mp-maptoref');` and add after `this.len = 0; ...` line:

```js
    this.seq2 = ''; this.seqLabel = 'nt'; this.seq2Label = 'Consensus';
```

2. `load(p)`: after `this.version = p.version; this.input = p.input;` add:

```js
    this.seq2 = p.seq2 || ''; this.seqLabel = p.seqLabel || 'nt'; this.seq2Label = p.seq2Label || 'Consensus';
```

3. Add a helper after `errOn`:

```js
  Viewer.prototype.ntOn = function () { return this.showNt && this.ppb >= NT_BAR; };
  Viewer.prototype.consOn = function () { return this.ntOn() && !!this.seq2; };
```

4. `height()`: replace the body with

```js
    var pcgs = this.showAa && this.ppb >= AA_MIN ? this.aaRows.length : 0;
    var nt = (this.ntOn() ? NT_H : 0) + (this.consOn() ? NT_H : 0);
    return this.topH() + this.nLanes * LANE_H + PAD + nt + pcgs * AA_H + PAD;
```

5. `draw()`: replace the line `if (this.showNt && this.ppb >= NT_BAR) { this.drawNt(c, y); y += NT_H; }` with

```js
    if (this.ntOn()) { this.drawNt(c, y, this.seq, this.seqLabel, null); y += NT_H; }
    if (this.consOn()) { this.drawNt(c, y, this.seq2, this.seq2Label, this.seq); y += NT_H; }
```

6. Replace `drawNt` entirely:

```js
  // One sequence row. With `ref`, each base is classed against the reference
  // (diffOverlay): mismatches get an amber outline, N a grey tile, "-" a hatch.
  Viewer.prototype.drawNt = function (c, y, seq, label, ref) {
    var vs = Math.floor(this.viewStart), ve = Math.ceil(this.viewStart + this.viewLen());
    var letter = this.ppb >= NT_LETTER, w = Math.max(1, this.ppb - (letter ? 1 : 0));
    var warn = cssVar('--mp-warning', '#8a5a00');
    c.textAlign = 'center'; c.textBaseline = 'middle';
    for (var lin = vs; lin <= ve; lin++) {
      var b = this.baseAt(lin); if (!b) continue;
      var ch = seq.charAt(b.pos - 1) || 'N';
      var cls = ref ? G.diffOverlay(ref.charAt(b.pos - 1), ch)[0] : 'same';
      var x = this.x(lin), col = cls === 'n' ? BASE.N : BASE[ch] || BASE.N;
      if (cls === 'gap') {
        c.save(); c.beginPath(); c.rect(x, y + 2, w, NT_H - 4); c.clip();
        c.strokeStyle = BASE.N; c.lineWidth = 1; c.beginPath();
        for (var d = 0; d < w + NT_H; d += 4) { c.moveTo(x + d, y + 2); c.lineTo(x + d - NT_H, y + NT_H - 2); }
        c.stroke(); c.restore();
      } else {
        c.fillStyle = col + (letter ? '99' : 'cc');
        c.fillRect(x, y + 2, w, NT_H - 4);
      }
      if (cls === 'mismatch') { c.lineWidth = 2; c.strokeStyle = warn; c.strokeRect(x + 1, y + 3, w - 2, NT_H - 6); }
      if (letter && cls !== 'gap') { c.fillStyle = '#000000'; c.fillText(ch, x + this.ppb / 2, y + NT_H / 2); }
    }
    c.lineWidth = 1;
    c.fillStyle = cssVar('--mp-text-muted', '#6a6a6a'); c.textAlign = 'right';
    c.fillText(label, GUTTER - 6, y + NT_H / 2);
  };
```

7. `drawTrack`: after `if (v === null) continue;` add the zero-depth mark:

```js
      if (!flag && v === 0) { c.fillStyle = muted; c.fillRect(px, y + h - 2, 1, 2); continue; }
```

8. Hover (`cv.addEventListener('mousemove', ...)`): after the `depth` line add

```js
      if (self.seq2) { var c2 = self.seq2.charAt(b.pos - 1); if (c2 && c2 !== b.base) t += ' | consensus ' + c2; }
```

9. `window.mpseq.state`: add `seq2Len: v.seq2.length,` after `height: v.height(),`.

- [ ] **Step 5: Run the JS tests to verify they pass**

Run: `CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh Rscript -e 'devtools::test(filter = "seqview-js")'`
Expected: all 13 pass.

- [ ] **Step 6: ASCII check and commit**

```bash
grep -nP '[^\x00-\x7F]' inst/app/www/seqviewer.js tests/testthat/test-seqview-js.R tests/testthat/seqview/index.html
git add inst/app/www/seqviewer.js tests/testthat/test-seqview-js.R tests/testthat/seqview/index.html
git commit -m "feat(seqview): consensus row with difference overlay"
```

---

### Task 3: JS reads mode

**Files:**
- Modify: `inst/app/www/seqviewer.js`
- Test: `tests/testthat/test-seqview-js.R`

**Interfaces:**
- Consumes: `G.readsCover`, `ntOn()`, `consOn()` from Task 2.
- Consumes: payload fields `readsInput` (Shiny input id) and `readsMaxBp` (int) from Task 1; reply message `mpseq_reads` with `{id, nonce, start, end, reads, mm, del, ins, nShown, nTotal}`.
- Produces: Shiny input `readsInput` set to `{start, end, nonce}` with `priority: 'event'`; `window.mpseq.state(id)` gains `readsWindow` (`{start, end}` or null), `nReads` (reads stored), `readRows` (lanes).

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-seqview-js.R`:

```r
test_that("reads mode requests a window under the cap, draws the reply, and clears on zoom out", {
  b <- sv_page()
  js(b, "(function(){
    window.__inputs = [];
    var seq = Array(2000).join('ACGT').slice(0, 2000);
    window.__handlers.mpseq({id:'sv-canvas', unit:'S1', len:2000, topology:'linear', seq:seq, version:1, selected:null, features:[],
      seq2:seq, seqLabel:'Reference', seq2Label:'Consensus', readsInput:'sv-reads', readsMaxBp:1000});
    window.mpseq.whole('sv-canvas');
    window.mpseq.goto('sv-canvas', 500);})()")
  Sys.sleep(0.4)
  r <- js(b, "(function(){
    var req = window.__inputs.filter(function(i){return i.name==='sv-reads';});
    var last = req[req.length-1];
    var h0 = window.mpseq.state('sv-canvas').height;
    window.__handlers.mpseq_reads({id:'sv-canvas', nonce: 999, start:1, end:1000, reads:[{row:1,start:1,end:900,strand:'+'}], mm:[], del:[], ins:[], nShown:1, nTotal:1});
    var stale = window.mpseq.state('sv-canvas').nReads;
    window.__handlers.mpseq_reads({id:'sv-canvas', nonce: last.value.nonce, start: last.value.start, end: last.value.end,
      reads:[{row:1,start:400,end:600,strand:'+'},{row:2,start:450,end:650,strand:'-'},{row:3,start:500,end:520,strand:'+'}],
      mm:[{row:2,pos:500,base:'T'}], del:[{row:1,start:410,end:412}], ins:[{row:3,pos:505,len:2}], nShown:3, nTotal:3});
    var s = window.mpseq.state('sv-canvas'), h1 = s.height;
    var hit = window.mpseq.hitTest('sv-canvas', 60 + (500 - s.viewStart) * s.ppb, h1 - 4 - 12 - 6);
    var n0 = req.length;
    window.mpseq.goto('sv-canvas', 520);
    var n1 = window.__inputs.filter(function(i){return i.name==='sv-reads';}).length;
    window.mpseq.whole('sv-canvas');
    var s2 = window.mpseq.state('sv-canvas');
    return JSON.stringify({n0:n0, req:last.value, h0:h0, h1:h1, stale:stale, nReads:s.nReads, rows:s.readRows, win:s.readsWindow,
      hitRow: hit && hit.row, n1:n1, cleared:s2.nReads, h2:s2.height});})()")
  Sys.sleep(0.4)
  s <- jsonlite::fromJSON(r)
  expect_equal(s$n0, 1)
  expect_lte(s$req$end - s$req$start + 1, 1000)
  expect_lte(s$req$start, 500); expect_gte(s$req$end, 500)
  expect_equal(s$stale, 0)
  expect_equal(s$nReads, 3); expect_equal(s$rows, 3)
  expect_equal(s$h1 - s$h0, 3 * 12 + 4)
  expect_equal(s$win$start, s$req$start)
  expect_equal(s$hitRow, 3)
  expect_equal(s$n1, s$n0)          # the reply covers the small pan; no new request
  expect_equal(s$cleared, 0)
  expect_lt(s$h2, s$h1)
  # zooming out past the cap sends nothing more
  n2 <- js(b, "window.__inputs.filter(function(i){return i.name==='sv-reads';}).length")
  expect_equal(n2, 1)
})

test_that("unchecking Reads drops the lanes and stops requests", {
  b <- sv_page()
  js(b, "(function(){
    window.__inputs = [];
    var seq = Array(2000).join('ACGT').slice(0, 2000);
    var box = document.getElementById('sv-show_reads'); box.checked = false; box.dispatchEvent(new Event('change'));
    window.__handlers.mpseq({id:'sv-canvas', unit:'S1', len:2000, topology:'linear', seq:seq, version:1, selected:null, features:[],
      readsInput:'sv-reads', readsMaxBp:1000});
    window.mpseq.goto('sv-canvas', 500);})()")
  Sys.sleep(0.4)
  n <- js(b, "window.__inputs.filter(function(i){return i.name==='sv-reads';}).length")
  expect_equal(n, 0)
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh Rscript -e 'devtools::test(filter = "seqview-js")'`
Expected: the first new test fails at `expect_equal(s$n0, 1)` (no request sent). The second may pass by accident; that is fine.

- [ ] **Step 3: Implement in `inst/app/www/seqviewer.js`**

1. Constants: add `READ_H = 12` to the `var RULER_H = 22, ...` line (10 px lane plus 2 px gap).

2. Constructor: after the `this.seq2 = ...` line from Task 2 add:

```js
    this.showReads = true; this.readsInput = null; this.readsMaxBp = 1000;
    this.reads = null; this.readsWindow = null; this.readsTimer = null; this.readsNonce = 0; this.readsSent = null;
```

3. `load(p)`: after the `this.seq2 = ...` line add:

```js
    this.readsInput = p.readsInput || null; this.readsMaxBp = p.readsMaxBp || 1000;
    if (!sameSeq) { this.reads = null; this.readsWindow = null; this.readsSent = null; }
```

4. After `consOn` add:

```js
  Viewer.prototype.readsOn = function () { return this.showReads && !!this.readsInput && this.viewLen() <= this.readsMaxBp; };
  Viewer.prototype.readRows = function () { return this.readsOn() && this.reads ? this.reads.rows : 0; };
  // Visible window clamped to the reference; wrapped positions on a circular
  // unit are not requested (R clamps anyway), which also keeps cover checks stable.
  Viewer.prototype.viewWindow = function () {
    return { start: Math.max(1, Math.floor(this.viewStart)),
             end: Math.min(this.len, Math.ceil(this.viewStart + this.viewLen())) };
  };
  // Ask R for reads after the view settles; the request is padded to the full
  // reads window so small pans stay inside the last reply.
  Viewer.prototype.requestReads = function () {
    var self = this;
    if (G.readsCover(this.readsWindow, this.viewWindow())) return;
    clearTimeout(this.readsTimer);
    this.readsTimer = setTimeout(function () {
      if (!self.readsOn() || !window.Shiny) return;
      var w = self.viewWindow();
      if (G.readsCover(self.readsWindow, w)) return;
      var pad = Math.max(0, Math.floor((self.readsMaxBp - (w.end - w.start + 1)) / 2));
      var req = { start: Math.max(1, w.start - pad), end: Math.min(self.len, w.end + pad) };
      if (self.readsSent && self.readsSent.start === req.start && self.readsSent.end === req.end) return;
      self.readsSent = req; req.nonce = ++self.readsNonce;
      window.Shiny.setInputValue(self.readsInput, req, { priority: 'event' });
    }, 150);
  };
  Viewer.prototype.loadReads = function (p) {
    if (p.nonce !== this.readsNonce) return;
    var rows = 0;
    (p.reads || []).forEach(function (r) { if (r.row > rows) rows = r.row; });
    this.reads = { reads: p.reads || [], mm: p.mm || [], del: p.del || [], ins: p.ins || [],
                   rows: rows, nShown: p.nShown, nTotal: p.nTotal };
    this.readsWindow = { start: p.start, end: p.end };
    this.draw();
  };
```

5. `height()`: change the return to

```js
    var rd = this.readRows() ? this.readRows() * READ_H + PAD : 0;
    return this.topH() + this.nLanes * LANE_H + PAD + nt + pcgs * AA_H + rd + PAD;
```

6. `draw()`: right after `this.clamp();` add

```js
    if (!this.readsOn()) { this.reads = null; this.readsWindow = null; this.readsSent = null; clearTimeout(this.readsTimer); }
```

and at the very end of `draw()` (after the `aaRows.forEach` line) add

```js
    if (this.readsOn()) { if (this.reads) this.drawReads(c, y); this.requestReads(); }
```

7. Add `drawReads` after `drawAa`:

```js
  Viewer.prototype.drawReads = function (c, y) {
    var W = this.wrap.clientWidth, self = this, mono = cssVar('--mp-font-mono', 'monospace');
    var fwd = cssVar('--mp-type-rrna', '#5DA5DA') + '99', rev = cssVar('--mp-type-ctrl', '#FAA34A') + '99';
    var letter = this.ppb >= NT_LETTER, h = READ_H - 2;
    var top = function (row) { return y + (row - 1) * READ_H; };
    c.textAlign = 'center'; c.textBaseline = 'middle';
    this.reads.reads.forEach(function (r) {
      var x0 = Math.max(GUTTER, self.x(r.start)), x1 = Math.min(W, self.x(r.end + 1));
      if (x1 <= x0) return;
      c.fillStyle = r.strand === '-' ? rev : fwd;
      c.fillRect(x0, top(r.row), x1 - x0, h);
      self.hits.push({ x0: x0, x1: x1, y0: top(r.row), y1: top(r.row) + h, read: r, row: r.row });
    });
    this.reads.del.forEach(function (d) {
      var x0 = Math.max(GUTTER, self.x(d.start)), x1 = Math.min(W, self.x(d.end + 1)), ym = top(d.row) + h / 2;
      if (x1 <= x0) return;
      c.fillStyle = cssVar('--mp-surface', '#ffffff'); c.fillRect(x0, top(d.row), x1 - x0, h);
      c.strokeStyle = '#555555'; c.lineWidth = 1;
      c.beginPath(); c.moveTo(x0, ym + 0.5); c.lineTo(x1, ym + 0.5); c.stroke();
    });
    if (this.ppb >= NT_BAR) {
      c.font = '9px ' + mono;
      this.reads.mm.forEach(function (m) {
        var x = self.x(m.pos);
        if (x + self.ppb < GUTTER || x > W) return;
        c.fillStyle = BASE[m.base] || BASE.N; c.fillRect(x, top(m.row), Math.max(1, self.ppb), h);
        if (letter) { c.fillStyle = '#000000'; c.fillText(m.base, x + self.ppb / 2, top(m.row) + h / 2); }
      });
    }
    c.font = '8px ' + mono; c.textAlign = 'left'; c.textBaseline = 'top';
    this.reads.ins.forEach(function (i) {
      var x = self.x(i.pos + 1);
      if (x < GUTTER || x > W) return;
      c.fillStyle = '#7b3fa0'; c.fillRect(x - 1, top(i.row), 2, h);
      if (letter && i.len > 1) c.fillText(String(i.len), x + 2, top(i.row));
    });
    c.font = '12px ' + mono; c.fillStyle = cssVar('--mp-text-muted', '#6a6a6a');
    c.textAlign = 'right'; c.textBaseline = 'middle';
    c.fillText('reads', GUTTER - 6, y + h / 2);
  };
```

8. `click()`: replace the two lines using `h.f` with

```js
    this.selected = h && h.f ? h.f.row : null;
    if (h && h.f && this.input && window.Shiny) {
```

9. Hover: change `if (h) t += ' | ' + h.f.gene ...` and the `notes` line to test `h && h.f`, and after the notes line add

```js
      if (h && h.read) {
        t += ' | read ' + h.read.strand + ' ' + h.read.start.toLocaleString() + '-' + h.read.end.toLocaleString();
        var mm = self.reads && self.reads.mm.find(function (m) { return m.row === h.row && m.pos === b.pos; });
        if (mm) t += ' mismatch ' + mm.base;
      }
```

10. `bindControls`: add `['show_reads', 'showReads']` to the checkbox pairs array.

11. Message handlers: add

```js
    window.Shiny.addCustomMessageHandler('mpseq_reads', function (p) { var v = get(p.id); if (v) v.loadReads(p); });
```

and make the `mpseq` handler retry once when the canvas is not in the DOM yet (the MapToRef canvas is inserted by a `uiOutput`):

```js
    window.Shiny.addCustomMessageHandler('mpseq', function (p) {
      var v = get(p.id);
      if (v) v.load(p); else setTimeout(function () { var w = get(p.id); if (w) w.load(p); }, 250);
    });
```

12. `window.mpseq.state`: add after `seq2Len`:

```js
             readsWindow: v.readsWindow, nReads: v.reads ? v.reads.reads.length : 0, readRows: v.readRows(),
```

- [ ] **Step 4: Run the JS tests to verify they pass**

Run: `CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh Rscript -e 'devtools::test(filter = "seqview-js")'`
Expected: all 15 pass. If the hit test (`hitRow`) misses, check the y arithmetic: the lanes start at `height() - PAD - readRows * READ_H - PAD`, so row 3's lane top is `h1 - 4 - 12 - 4 - 4 + 2 * 12`... simply compute it in the test as `h1 - 4 - 12 + 2` if needed and keep the assertion on row 3.

- [ ] **Step 5: ASCII check and commit**

```bash
grep -nP '[^\x00-\x7F]' inst/app/www/seqviewer.js tests/testthat/test-seqview-js.R
git add inst/app/www/seqviewer.js tests/testthat/test-seqview-js.R
git commit -m "feat(seqview): on-demand read lanes"
```

---

### Task 4: Rewrite the MapToRef viewer module, CSS, NEWS

**Files:**
- Rewrite: `R/app_assemble_maptoref_viewer.R` (whole file)
- Modify: `inst/app/www/custom.css` lines 835-945 (MapToRef block)
- Modify: `NEWS.md` line 20 (the MapToRef coverage viewer bullet)
- Modify: `tests/testthat/test-maptoref-viz-data.R` (delete the `.mtr_zero_runs` test at the end)

**Interfaces:**
- Consumes: `maptoref_seqview_payload`, `maptoref_reads_reply`, `MTR_READS_MAX_BP` (Task 1); JS messages `mpseq`, `mpseq_reads`, input `reads_req` (Task 3).
- Consumes: `mp_checkbox(inputId, label, value)` from `R/app_ui_helpers.R`; `on()` gargoyle wrapper; `maptoref_paths`, `maptoref_read_*`, `.mtr_ref_now`, `.mtr_ref_key` (existing).
- Produces: `maptoref_viewer_ui(id)`, `maptoref_viewer_server(id, rv)` (same signatures as today, mounted from `R/app_assemble_coverage_details.R:185,220`).

- [ ] **Step 1: Delete the `.mtr_zero_runs` test**

Remove the final `test_that(".mtr_zero_runs merges adjacent gaps and pads short ones", ...)` block from `tests/testthat/test-maptoref-viz-data.R`.

- [ ] **Step 2: Replace `R/app_assemble_maptoref_viewer.R` with**

```r
#' Placeholder for the MapToRef sequence viewer
#'
#' Rendered inside the assembly details modal; empty for samples that were not
#' assembled with MapToRef.
#'
#' @param id module id
#' @return a uiOutput
#'
#' @noRd
maptoref_viewer_ui <- function(id) {
  uiOutput(shiny::NS(id, "view"))
}

#' MapToRef results in the sequence viewer (tools/maptoref_seqview_spec.md)
#'
#' @noRd
maptoref_viewer_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    state <- reactiveValues(
      paths = NULL, depth = NULL, features = NULL, summary = NULL,
      ref_seq = NA_character_, cons_seq = NA_character_, len = 0L,
      ref_now = NA_character_, has_work = FALSE
    )
    version <- reactiveVal(0L)
    reads_note <- reactiveVal(NULL)

    on("coverage_modal", {
      p <- maptoref_paths(
        session$userData$dir_out, rv$updating$ID, rv$updating$assemble_opts
      )
      state$paths <- p
      state$has_work <- dir.exists(p$work)
      state$depth <- maptoref_read_depth(p$depth)
      state$features <- maptoref_read_features(p$features)
      state$summary <- maptoref_read_summary(p$summary)
      state$ref_now <- .mtr_ref_now(session$userData$con, rv$updating$ID)
      state$ref_seq <- maptoref_read_seq(p$ref_fasta)
      state$cons_seq <- maptoref_read_seq(p$consensus)
      state$len <- nrow(state$depth)
      reads_note(NULL)
      version(isolate(version()) + 1L)
    })

    has_bam <- reactive(isTRUE(file.exists(state$paths$bam %||% "")))
    cons_mismatch <- reactive(
      !is.na(state$cons_seq) && !is.na(state$ref_seq) &&
        nchar(state$cons_seq) != nchar(state$ref_seq)
    )

    output$view <- renderUI({
      if (!isTRUE(state$has_work)) {
        return(NULL)
      }
      if (state$len == 0L) {
        return(tags$div(
          class = "mp-maptoref",
          tags$b("MapToRef reference coverage"),
          tags$div(
            class = "mp-coverage-caption",
            "No coverage table for this sample. Run Update on it to produce ",
            "the coverage and read files."
          )
        ))
      }
      btn <- function(action, label, icon = NULL, title = NULL) {
        tags$button(
          type = "button", class = "btn btn-default", `data-mpseq` = action,
          title = title, `aria-label` = title %||% label, icon, label
        )
      }
      goto <- numericInput(ns("goto"), NULL, value = NA, min = 1, step = 1, width = "130px")
      goto <- htmltools::tagQuery(goto)$find("input")$
        addAttrs(placeholder = "Go to position", `aria-label` = "Go to position")$allTags()
      reads_box <- mp_checkbox(ns("show_reads"), label = "Reads", value = TRUE)
      if (!has_bam()) reads_box <- shinyjs::disabled(reads_box)
      tags$div(
        class = "mp-maptoref",
        tags$b("MapToRef reference coverage"),
        uiOutput(ns("header")),
        div(
          class = "mp-seqview-controls",
          goto,
          btn("whole", "Whole genome"),
          btn("zoom_in", NULL, icon("magnifying-glass-plus"), "Zoom in"),
          btn("zoom_out", NULL, icon("magnifying-glass-minus"), "Zoom out"),
          mp_checkbox(ns("show_cov"), label = "Coverage", value = TRUE),
          mp_checkbox(ns("show_nt"), label = "Nucleotides", value = TRUE),
          reads_box
        ),
        div(class = "mp-coverage-caption",
            paste("Drag or scroll sideways to pan, scroll or pinch to zoom.",
                  "Letters appear when zoomed in; reads appear under 1,000 bp.")),
        uiOutput(ns("note")),
        div(
          class = "mp-seqview",
          tags$canvas(id = ns("canvas"), class = "mp-seqview-canvas"),
          tags$div(id = ns("tip"), class = "mp-maptoref-tip", hidden = NA)
        )
      )
    })

    output$header <- renderUI({
      s <- state$summary
      # Older runs never wrote every key (e.g. reference_source); omit the
      # field rather than print a "not recorded" placeholder for it.
      fld <- function(k) {
        v <- unname(s[k])
        if (is.null(v) || is.na(v)) NA_character_ else v
      }
      ref_len <- fld("reference_length")
      n_pct <- suppressWarnings(
        round(100 * as.numeric(fld("n_count")) / as.numeric(ref_len), 1)
      )
      item <- function(label, value) {
        if (is.null(value) || is.na(value)) return(NULL)
        tags$span(class = "mp-maptoref-field", tags$b(label), " ", value)
      }
      tags$div(
        class = "mp-maptoref-meta",
        item("Reference:", fld("accession")),
        item("Organism:", fld("organism")),
        item("Length:", if (!is.na(ref_len)) paste0(ref_len, " bp")),
        item("Source:", fld("reference_source")),
        item("Reads mapped:", fld("reads_mapped_final")),
        item("Mean depth:", round(mean(state$depth$Depth), 1)),
        item("Uncalled bases:", if (!is.na(n_pct)) paste0(n_pct, "%")),
        # The run records the reference it was given; a different value on the
        # sample now means this assembly is behind its settings.
        if (!is.na(fld("reference")) && !is.na(state$ref_now) &&
            .mtr_ref_key(fld("reference")) != .mtr_ref_key(state$ref_now)) {
          tags$span(class = "mp-maptoref-field mp-maptoref-stale",
                    tags$b("Reference changed since this assembly:"),
                    paste0(" now ", state$ref_now, ". Run Update on the sample to re-map."))
        },
        if (nrow(state$features) == 0L) {
          tags$span(class = "mp-maptoref-field mp-maptoref-nofeat",
                    "Reference has no annotation record.")
        },
        if (cons_mismatch()) {
          tags$span(class = "mp-maptoref-field mp-maptoref-nofeat",
                    "Consensus row: not shown (length differs from the reference).")
        }
      )
    })

    # The canvas lives inside output$view, so the payload goes out after the
    # flush that inserts it.
    observe({
      req(isTRUE(state$has_work), state$len > 0L)
      p <- maptoref_seqview_payload(
        state$depth, state$features, state$ref_seq, state$cons_seq,
        unname(state$summary["reference_topology"]), rv$updating$ID, version()
      )
      p$id <- ns("canvas")
      if (has_bam()) p$readsInput <- ns("reads_req")
      session$onFlushed(function() session$sendCustomMessage("mpseq", p), once = TRUE)
    })

    observeEvent(input$reads_req, {
      r <- input$reads_req
      req(state$len > 0L, has_bam())
      start <- max(1L, as.integer(r$start))
      end <- min(state$len, as.integer(r$end))
      w <- maptoref_window_reads(state$paths$bam, start, end, state$ref_seq)
      reply <- maptoref_reads_reply(w, start, end, r$nonce)
      reply$id <- ns("canvas")
      session$sendCustomMessage("mpseq_reads", reply)
      rng <- paste0(format(start, big.mark = ","), "-", format(end, big.mark = ","))
      reads_note(if (w$n_total == 0L) {
        paste0("No reads in ", rng, ".")
      } else {
        paste0(
          "Showing ", format(w$n_shown, big.mark = ","), " of ",
          format(w$n_total, big.mark = ","), " reads in ", rng, ".",
          if (w$n_shown < w$n_total) " The deepest 100 rows are shown."
        )
      })
    })

    output$note <- renderUI({
      if (!has_bam()) {
        return(div(class = "mp-coverage-caption",
                   "No read alignments were kept for this sample. Run Update on it to keep them."))
      }
      n <- reads_note()
      if (is.null(n)) NULL else div(class = "mp-coverage-caption", n)
    })
  })
}
```

- [ ] **Step 3: CSS**

In `inst/app/www/custom.css`, in the MapToRef block (starts at the comment `/* MapToRef coverage and pileup viewer */`):
- Change the header comment to `/* MapToRef sequence viewer section (R/app_assemble_maptoref_viewer.R). */`.
- Delete the rules `.mp-maptoref-controls`, `.mp-maptoref-controls .form-group`, `.mp-maptoref-plot`, `.mp-maptoref-pileup`, `.mp-maptoref-pileup-col`, `.mp-maptoref-scrollbar`, `.mp-maptoref-scrollpane`, the combined `.mp-maptoref-scrollbar, .mp-maptoref-scrollpane` rule, and the two `::-webkit-scrollbar` rules, with their comments.
- Keep `.mp-maptoref`, `.mp-maptoref-meta`, `.mp-maptoref-field`, `.mp-maptoref-nofeat`, `.mp-maptoref-stale`, `.mp-maptoref-tip`.
- Verify: `grep -c "mp-maptoref-" inst/app/www/custom.css` prints 5 and `grep -rn "mp-maptoref-\(controls\|plot\|pileup\|scroll\)" R/ inst/` prints nothing.

- [ ] **Step 4: NEWS**

Replace the `- **MapToRef coverage viewer.** ...` bullet in `NEWS.md` (line 20) with:

```
- **MapToRef sequence viewer.** A button on MapToRef samples opens the sequence viewer on the reference: read depth across the reference with uncovered positions marked, a gene track when the reference is a GenBank record, the reference and consensus sequences with differences outlined, and, once zoomed under 1,000 bp, the individual reads with mismatches, insertions, deletions, and strand. MapToRef now keeps its final read alignment so the reads can be drawn.
```

- [ ] **Step 5: Run the R tests**

Run: `Rscript -e 'devtools::document(); devtools::test(filter = "maptoref")'`
Expected: 0 failures. Then the full suite: `Rscript -e 'devtools::test()'` and expect `FAIL 1 | WARN 4` (the pre-existing `test-ambiguous-cds-translation.R:53`).

- [ ] **Step 6: Sanity-load the module**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); ui <- maptoref_viewer_ui("x"); stopifnot(inherits(ui, "shiny.tag")); cat("ok\n")'`
Expected: `ok`. Also `grep -n "gggenes\|ggplot2::\|scales::" R/app_assemble_maptoref_viewer.R` prints nothing.

- [ ] **Step 7: ASCII check and commit**

```bash
grep -nP '[^\x00-\x7F]' R/app_assemble_maptoref_viewer.R inst/app/www/custom.css NEWS.md tests/testthat/test-maptoref-viz-data.R
git add R/app_assemble_maptoref_viewer.R inst/app/www/custom.css NEWS.md tests/testthat/test-maptoref-viz-data.R
git commit -m "feat(maptoref): draw results in the sequence viewer"
```

---

### Task 5: Live verification against a real MapToRef project

**Files:**
- Create: `dev/ui_review/steps/maptoref_seqview_1.R` (git-ignored harness script)
- Fix anything found in the files above; commit fixes with `fix(maptoref): <what>`.

**Interfaces:**
- Consumes: the harness in `dev/ui_review/` (`app.sh`, `capture_lib.R`, `HARNESS_README.md`) and the project `~/Documents/DJMCompBio/MitoPilot_testing/2026-09-14_maptoref` (sample `MULTISCAFF` on parameter set `mapToRef` has a GenBank reference with 37 features, a consensus, and `final.bam`).

- [ ] **Step 1: Write the harness script**

`dev/ui_review/steps/maptoref_seqview_1.R`:

```r
nav(wait = 8); wait_for(ROWS_ASM); Sys.sleep(3)
swal_accept(wait = 2)
click_in_row("MULTISCAFF", 3, rows = ROWS_ASM); Sys.sleep(15)
swal_accept(wait = 2)
cat("modal:", js("document.querySelectorAll('.modal-dialog').length"), "\n")
cat("viewer:", js("document.querySelectorAll('.mp-maptoref').length"), "\n")
CV <- js("(function(){var c=document.querySelector('.mp-maptoref canvas'); return c? c.id : 'none';})()")
cat("canvas:", CV, "\n")
st <- function() js(sprintf("JSON.stringify(window.mpseq.state('%s'))", CV))
cat("meta:", js("(document.querySelector('.mp-maptoref-meta')||{}).innerText"), "\n")
cat("state whole:", substr(st(), 1, 300), "\n")
js("document.querySelector('.mp-maptoref').scrollIntoView({block:'start'}); 'ok'"); Sys.sleep(1)
shot_modal("maptoref-seqview-whole")
js(sprintf("window.mpseq.goto('%s', 3000)", CV)); Sys.sleep(6)
cat("state reads:", st(), "\n")
cat("note:", js("(document.querySelector('.mp-maptoref .mp-coverage-caption + .shiny-html-output')||{}).innerText"), "\n")
shot_modal("maptoref-seqview-reads")
js(sprintf("window.mpseq.zoom('%s', 0.25)", CV)); Sys.sleep(6)
cat("state zoomed out:", substr(st(), 1, 300), "\n")
shot_modal("maptoref-seqview-bars")
js(sprintf("window.mpseq.goto('%s', 1)", CV)); Sys.sleep(6)
cat("state origin:", substr(st(), 1, 300), "\n")
shot_modal("maptoref-seqview-origin")
cat("errors:", js("(window.__mp_errs||[]).join(' | ')"), "\n")
b$close()
```

- [ ] **Step 2: Start the app and run the script**

```bash
H=$PWD/dev/ui_review
$H/app.sh start ~/Documents/DJMCompBio/MitoPilot_testing/2026-09-14_maptoref 3857
cd $H && APP_PORT=3857 SHOT_DIR=$H/shots/maptoref_seqview CHROMOTE_CHROME=$H/chrome-wrap.sh Rscript capture_lib.R steps/maptoref_seqview_1.R
$H/app.sh stop 3857
```

Expected: `viewer: 1`, a canvas id ending in `-maptoref_viewer-canvas`, `state whole` with `len` around 16-17 kb and `nLanes >= 2`, `state reads` with `nReads > 0` and `readRows > 0` and a note reading `Showing N of M reads in ...`, `state zoomed out` with `nReads 0` when the view exceeds 1,000 bp, and no R errors in `dev/ui_review/app_logs/app_3857.log`.

- [ ] **Step 3: Inspect the screenshots**

Read `dev/ui_review/shots/maptoref_seqview/*.png`. Check: depth track with zero-depth marks, feature arrows coloured by type, Reference and Consensus rows with amber outlines on differences at letter zoom, read lanes in blue and orange under them, the whole modal scrolling (no inner scrollbar), the tooltip hidden by default. Fix and re-run until clean.

- [ ] **Step 4: Full test run and commit any fixes**

```bash
Rscript -e 'devtools::test()'
CHROMOTE_CHROME=$PWD/dev/ui_review/chrome-wrap.sh Rscript -e 'devtools::test(filter = "seqview-js")'
git status --short
```

Expected: `FAIL 1 | WARN 4`, JS tests all pass, only intended files changed.
