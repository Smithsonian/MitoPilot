# MapToRef results in the sequence viewer

Spec, 2026-09-14. Branch `feat/maptoref-seqview` (off `map-to-ref-assembly`).
Approved design: the MapToRef "Coverage Map" and "Read Pileup" panels in the
assembly details window are replaced by one canvas driven by the existing
sequence viewer (`inst/app/www/seqviewer.js`, `tools/nt_viewer_spec.md`), which
gains an optional reads mode.

## 1. Purpose

Today a MapToRef sample shows its result as two R plots: a coverage map with
brush-to-zoom and a server round-trip per hover, and a separate fixed-width
pileup plot scrolled sideways through a synthetic scrollbar. The annotation
details window already has a canvas viewer with wheel zoom from whole genome to
letters, gene lanes, a depth track, and hover. One viewer for both windows means
one navigation model for the user and one drawing engine to maintain, and it
removes the pileup's scroll glue.

## 2. Scope

In scope: the MapToRef section of the assembly details window. One canvas with
ruler, depth track, GenBank feature lanes, reference row, consensus row with
differences marked, and read lanes that appear automatically when the view is
1,000 bp wide or narrower. Header metadata and warnings kept. Reads still read
from the BAM by the existing R reader.

Out of scope: the annotate viewer's behaviour (unchanged unless a field in the
payload asks for the new rows), editing, exporting, error-rate track for
MapToRef (no per-base error file exists), amino-acid rows for MapToRef
(GenBank features carry no stored translation in this app), base-quality
shading, paired-read linking, soft-clip display.

## 3. Placement and controls

`maptoref_viewer_ui()` keeps its outer `div.mp-maptoref` with the bold title
and `uiOutput(header)`. The two `tags$details` sections go. In their place:

```
div.mp-seqview-controls
  goto (numeric, placeholder "Go to position")
  Whole genome | Zoom in | Zoom out     (data-mpseq buttons, as Annotate)
  Coverage | Nucleotides | Reads        (mp_checkbox, all TRUE by default)
div.mp-coverage-caption
  "Drag or scroll sideways to pan, scroll or pinch to zoom. Letters appear when
   zoomed in; reads appear under 1,000 bp."
uiOutput(note)          (reads count / no-BAM message, see 6)
div.mp-seqview
  canvas#<ns>-canvas.mp-seqview-canvas
  div#<ns>-tip.mp-maptoref-tip[hidden]
```

No "Fit gene" (no table to select from), no amino-acid box, no error-rate box.
The section is not collapsible: the assembly details window already scopes it.
Controls reuse the `mp-seqview-*` CSS block; the `.mp-maptoref-plot`,
`.mp-maptoref-pileup*`, `.mp-maptoref-scroll*` rules are deleted.

## 4. Data contract (R to browser)

### 4.1 Load payload, message `mpseq`

Built by a pure function `maptoref_seqview_payload(depth, features, ref_seq,
cons_seq, topology, unit, version)` in `R/maptoref_viz_data.R` and sent by the
viewer module on open and on refresh. Same field names as the annotate payload
where the meaning is the same:

| field | type | value |
|---|---|---|
| `id` | string | canvas element id |
| `unit` | string | sample ID |
| `len` | int | `nchar(ref_seq)` |
| `topology` | "circular" or "linear" | from the summary's `topology` key, else "linear" |
| `seq` | string | reference sequence, upper case (`ref.fasta`) |
| `seq2` | string or absent | consensus (`subs_only.fasta`), upper case; absent when the file is missing or its length differs from `len` (an indel-bearing consensus cannot be laid over the reference position for position; the hover then says so) |
| `seq2Label` | string | "Consensus" |
| `seqLabel` | string | "Reference" |
| `version` | int | bumped on every refresh of the window, so the browser refits |
| `features` | array | one per row of `maptoref_features.csv`: `row` (1-based), `type`, `gene`, `pos1 = start`, `pos2 = end`, `dir = strand`, `partial5 = partial3 = false`, `notes = ""` |
| `depth` | int array | per-base depth from `maptoref_depth.csv`, indexed by position |
| `input` | string | Shiny input id for feature clicks; absent here (no table to select), so clicks do nothing |
| `readsInput` | string | Shiny input id the browser sets to request reads (see 4.2); absent when there is no BAM, which turns the reads mode off |
| `readsMaxBp` | int | 1000 |

A feature whose `start > end` is not produced by the GenBank parser (it splits
origin-crossing joins), so `pos1 <= pos2` always holds here; the viewer's
wrap handling is simply not exercised by this payload.

### 4.2 Reads request, Shiny input `readsInput`

When the reads mode is on and the view width is at most `readsMaxBp`, the
browser sets `readsInput` to `{ start, end, nonce }` (1-based, inclusive, on
the reference; `start` may be below 1 or `end` above `len` on a circular unit,
and the R side clamps to `[1, len]`). Sent with `priority: "event"`, debounced
150 ms after the last zoom or pan, and skipped when the requested window is
inside the window of the last reply. Zooming out past `readsMaxBp` clears the
lanes and sends nothing.

### 4.3 Reads reply, message `mpseq_reads`

The R side calls `maptoref_window_reads(bam, start, end, ref_seq)` (unchanged,
including the circular-flank second scan and the 100-row cap) and sends:

| field | value |
|---|---|
| `id` | canvas element id |
| `nonce` | echoed from the request; a reply whose nonce is not the latest sent is dropped |
| `start`, `end` | the window actually read |
| `reads` | columns `{ row: [], start: [], end: [], strand: [] }` (`row` 1-based lane); the browser expands to one object per read |
| `mm` | columns `{ row: [], pos: [], base: [] }` |
| `del` | columns `{ row: [], start: [], end: [] }` |
| `ins` | columns `{ row: [], pos: [], len: [] }` |
| `nShown`, `nTotal` | ints |

Read names are not sent (hover shows strand and span; names are not needed
and cost the most bytes). A 1,000 bp window with 100 rows is under 60 KB.

## 5. Drawing

Tracks, top to bottom, on the shared x scale of the sequence viewer:

1. Ruler (unchanged).
2. Depth track (unchanged, drawn when `depth` is present and the Coverage box is on). Positions with zero depth are additionally underlined in the ruler colour at every zoom, so gaps read at whole-genome scale.
3. Feature lanes (unchanged; GenBank types map to the same colour tokens as the annotation table's PCG, tRNA, rRNA, and "other" badges; unknown types use "other").
4. Reference row, labelled "Reference" in the gutter: the existing nucleotide row (letters at `pxPerBase >= 8`, bars from 3, hidden below).
5. Consensus row, labelled "Consensus", drawn only when `seq2` is present and the row would be visible: same tiles as the reference row, with a difference overlay per base: mismatch gets a 2 px outline in `--mp-warning`, `N` gets a grey fill regardless of the reference base, `-` (a deletion in the consensus) gets a hatched tile. At bar zoom (3 to 8 px) only the outline and grey survive; below 3 px the row is hidden with the reference row.
6. Read lanes, drawn only when the reads mode is on, the view is at most `readsMaxBp` wide, and a reply covering the view has arrived. One 10 px lane per `row`, 2 px gap, forward reads in `--mp-type-rrna` (blue) at 60% alpha, reverse reads in `--mp-type-ctrl` (orange) at 60% alpha, read once from the CSS custom properties like the feature colours; a read is a rounded bar from `start` to `end`, with mismatches as base-coloured tiles bearing the letter at letter zoom, deletions as a 1 px line across the gap, insertions as a 2 px purple tick at `pos` with a superscript `len` when `len > 1` at letter zoom. Lanes are clipped to the canvas width; the canvas height grows with the number of lanes (as amino-acid rows grow it today) so the whole modal scrolls, no inner scrollbar.

Colours for bases stay the ones the pileup uses today (A green, C blue, G
orange, T red, N grey), already the viewer's palette.

## 6. Notes and states

`uiOutput(note)` above the canvas, rendered by R:

- Reads mode on and a reply arrived: "Showing N of M reads in <start>-<end>." When `nShown < nTotal`: " Reads are packed into rows and only the first 100 rows are drawn; the other K reads are not shown." The browser sends `readsInput`; R updates a `reactiveVal` with the reply counts, so this note is server-rendered like the header.
- No BAM file: "No read alignments were kept for this sample. Run Update on it to keep them." and `readsInput` is absent from the payload (reads box disabled).
- No coverage table (`maptoref_depth.csv` missing): the whole section shows the existing caption "No coverage table for this sample. Run Update on it to produce the coverage and read files." and no canvas, as today.
- No features file: lanes are empty and the header keeps its existing "Reference has no annotation record." field.
- `seq2` absent because of a length mismatch: the header gains a field "Consensus row: not shown (length differs from the reference)".

The reference-changed warning in the header is unchanged.

## 7. Navigation and hover

Wheel, pinch, drag, shift-wheel, buttons, and goto exactly as the annotate
viewer. `goto` centres at 10 px per base, so reads load on arrival.

Hover tooltip, one line: `Position 12,345, A | depth 87 | consensus G` (when
`seq2` present and differs) `| COX1 (PCG)` (feature under the pointer) and,
when the pointer is over a read lane, `| read + 12,200-12,350` with any
mismatch letter under the pointer appended as `mismatch T`.

Feature clicks do nothing here (no `input` in the payload).

## 8. JS changes (`inst/app/www/seqviewer.js`)

- `load(p)`: store `seq2`, `seqLabel`, `seq2Label`, `readsInput`, `readsMaxBp`; `showReads` defaults to true. Existing behaviour when these are absent is unchanged (the annotate payload never sends them).
- `height()`: add the consensus row and the read lanes.
- `draw()`: after the nucleotide row, draw the consensus row (5.5) and read lanes (5.6).
- New `requestReads()` called from the end of `clamp()`-driven redraws: computes the visible window, applies the debounce and the covered-window skip, and calls `Shiny.setInputValue(readsInput, {start, end, nonce}, {priority: 'event'})`.
- New message handler `mpseq_reads` that stores the reply (dropping stale nonces) and redraws.
- `hitTest()`: add read-lane hits (`{ read, row }`) so the hover can show them.
- `bindControls()`: `show_reads` checkbox wired like `show_nt`; unchecking clears stored reads and stops requests.
- Pure helpers, exposed on `window.mpseq.geom` for the JS tests: `diffOverlay(a, b)` returning the class per position ("same", "mismatch", "n", "gap"), and `readsCover(have, want)` returning whether `have` covers `want`.
- `window.mpseq.state(id)` gains `readsWindow` and `nReads` for the harness.

Estimated 200 to 260 lines added; nothing existing removed.

## 9. R changes

- `R/maptoref_viz_data.R`: add `maptoref_seqview_payload()` (pure) and `maptoref_reads_reply(w, start, end, nonce)` (pure: renames the reader's frames to the 4.3 shape).
- `R/app_assemble_maptoref_viewer.R`: rewrite the module. Keeps `state` (paths, depth, features, summary, ref_seq, cons_seq, len, ref_now, has_work), the header renderer and its warnings, the `coverage_modal` load. Removes `win_range`, `set_window`, `pileup_range`, `pileup_data`, all zoom/brush/click observers, `output$tracks`, `output$tooltip`, `output$pileup_ui`, `output$pileup`, `output$pileup_labels`, `output$pileup_note`, and the drawing helpers `.mtr_zero_runs`, `.mtr_view_tracks`, `.mtr_pileup_ylim`, `.mtr_view_pileup_labels`, `.mtr_view_pileup`, plus the `MTR_VIEW_*` and `MTR_PILEUP_*` constants. Adds: a `version` reactiveVal bumped on `coverage_modal`, an observer that sends `mpseq`, an `observeEvent(input$reads_req)` that calls `maptoref_window_reads()`, updates a `reads_note` reactiveVal, and sends `mpseq_reads`, and `output$note`.
- `R/app_assemble_coverage_details.R`: the mount points are unchanged.
- `inst/app/www/custom.css`: delete the plot, pileup, scrollbar, and scrollpane rules under "MapToRef"; keep `.mp-maptoref`, `-meta`, `-field`, `-nofeat`, `-stale`, `-tip`. `.mp-maptoref-controls` goes (the seqview controls block is used).
- `tests/testthat/test-maptoref-viz-data.R`: keep the reader tests, drop the plotting-helper tests (below); add payload tests (feature mapping, `seq2` dropped on length mismatch, depth indexed by position, no `readsInput` without a BAM) and a reply-shape test.
- `tests/testthat/test-seqview-js.R`: add `diffOverlay` and `readsCover` cases; add one page test that loads a payload with `seq2` and a reads reply and checks `state().nReads` and the height growth.
- `tests/testthat/test-maptoref-viz-data.R`: delete the tests that exercised the removed R plotting helpers (`.mtr_view_tracks`, `.mtr_view_pileup`, `.mtr_zero_runs`, `.mtr_pileup_ylim`); they live in this file today.
- `dev/ui_review/steps/maptoref_viewer_*.R`: rewrite the three harness scripts to drive `window.mpseq.zoom/goto` and read `window.mpseq.state()`.
- `NEWS.md`: one bullet under the MapToRef section.

## 10. Error handling

- BAM unreadable or index missing and unbuildable: the reader returns its empty result; reply has `nTotal = 0`, note says "No reads in <start>-<end>."
- A reply arriving after the view moved: dropped by nonce; the next settle sends a fresh request.
- `depth` shorter than `len`: missing positions are `null` and drawn as gaps (existing track behaviour).
- Payload send failures: as the annotate spec, the browser keeps the last good state.

## 11. Performance

Whole genome: depth track binned by the existing track drawer, 40 arrows. Letter
zoom: two sequence rows of about 200 letters plus at most 100 read lanes of a
few hundred tiles each, one frame. Reads request: one `scanBam` over at most
1,000 bp per settle, the same cost as today's pileup per click, but now also on
every pan at deep zoom; the 150 ms debounce and covered-window skip keep it to
one call per gesture.

## 12. Not changed

`maptoref_window_reads()`, `.mtr_cigar_walk()`, `.mtr_stack_rows()`, the
depth and features files, the pipeline, the header fields, the annotate
viewer's visible behaviour.
