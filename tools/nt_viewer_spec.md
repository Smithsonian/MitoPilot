# Sequence viewer for the annotation details window

Spec, 2026-09-12. Approved design: a custom canvas viewer (no library) showing the
unit's nucleotide sequence, its gene annotations in lanes, and one amino-acid row
per protein-coding gene, inside the annotation details window, kept in step with
every edit the window makes.

## 1. Purpose

A reviewer working in the annotation details window can today see features as
rows, copy a gene's nucleotide or amino-acid string, and inspect one alignment at
a time. They cannot see the sequence itself with the annotations laid over it, so
questions like "is this start codon really where the arrow says", "do these two
genes overlap by a codon", or "what sits in this gap" need an outside tool. The
viewer answers them in place and follows the edits (delete, merge, un-join,
boundary nudges, linearize, trim, restore) as they happen.

## 2. Scope

In scope: one unit (ID, path, scaffold) at a time, the one the details window is
open on. Nucleotide row, amino-acid rows for PCGs, gene lanes, ruler, semantic
zoom from whole genome to letters, pan, hover, click-to-select, jump from the
annotation table, circular and linear units, live refresh on edits.

Out of scope: editing from the viewer (edits stay in the existing controls),
read coverage (the Coverage Map section keeps that), reference synteny, tRNA
secondary structure, exporting images, multi-unit views, translation in the
browser (the stored translations are the truth).

## 3. Placement and controls

A new collapsible section, `tags$details(id = "sequence_details")` with summary
"Sequence", placed in `annotate_details_modal()` directly after the annotation
table's action row (`annotation_btns_wrapper`) and before the `tags$hr()` that
precedes Coverage Map. Closed by default. It opens by itself the first time the
user clicks a gene row in the annotation table (see 7), or when expanded by hand.

Controls, one row above the canvas, in the app's form style (labels with colons,
`btn-default` buttons, `mp_checkbox`):

- `Nucleotides` and `Amino acids` checkboxes, both on by default.
- `Position:` numeric input (1-based); Enter centres the view on it.
- `Fit gene` (enabled when a gene is selected), `Whole genome`, zoom in, zoom out
  (icon buttons with titles and aria-labels, as the MapToRef viewer's).
- A caption under the controls in `.mp-coverage-caption`: "Drag to pan, scroll or
  pinch to zoom; click a gene to select its row. Letters appear when zoomed in."

The section is read-only with respect to the lock: nothing in it writes.

## 4. Data contract (R to browser)

The R module builds one payload and sends it with
`session$sendCustomMessage("mpseq", payload)` whenever any input to it changes
(see 8). Fields:

| field | type | source |
|---|---|---|
| `id` | string | the canvas element id (namespaced) |
| `unit` | string | `ID.path.scaffold` label |
| `len` | int | `nchar(sequence)` |
| `topology` | "circular" or "linear" | `rv$updating$topology` |
| `seq` | string | the unit's sequence, upper case, from `rv$editing$assembly` when an edit session is open, else `get_assembly(ID, path, scaffold, con)[[1]]` |
| `version` | int | `asmb_edit_tick()` plus a modal-open nonce; a change means the sequence may have changed |
| `features` | array | one object per row of `rv$annotations` whose `pos1 > 0` (soft-deleted rows have `pos1 == 0`, see below) |
| `selected` | int or null | row index of the feature to highlight (table selection or the feature under edit) |

Each feature object: `row` (1-based index into `rv$annotations`, the highlight
key), `type` (PCG, tRNA, rRNA, or the stored type), `gene`, `pos1`, `pos2`
(1-based, inclusive, `pos1 > pos2` means the feature crosses the origin of a
circular unit), `dir` ("+" or "-"), `partial5`, `partial3` (booleans from
`partial_start`/`partial_stop`), `translation` (string or null; only PCGs),
`joined` (the `JOIN:` marker from notes, so exon pieces of one gene share a
label), `notes` (first 80 characters, for the hover).

Coordinates are never converted on the R side; the browser owns the modular
arithmetic. The payload for a 17 kb unit with 40 features is about 20 KB.

## 5. Drawing

One `<canvas>` sized to its container width (resize observer) and to the sum of
the track heights, drawn on `requestAnimationFrame`, everything in device pixels
for crisp text. Shared x scale `px = (pos - viewStart) * pxPerBase` with
`viewStart` in [1, len] and `pxPerBase` in [width / len, 14].

Tracks, top to bottom:

1. **Ruler.** Tick labels in bases (1-based), tick spacing chosen from
   1/2/5 x 10^k to give about one label per 90 px. On a circular unit the origin
   is marked with a vertical rule and "1" whenever it is in view.
2. **Gene lanes.** Arrows (pointed by strand) with the gene name inside when it
   fits, else nothing (the hover has it). Colours: the same fill per type as the
   annotation table's type badges, read once from CSS custom properties so the
   palette has one source. Lane assignment: sort features by start position
   along the linearised coordinate, place each in the first lane whose last end
   is before its start (greedy interval packing, the same rule as the MapToRef
   pileup's row stacking). A feature crossing the origin is drawn as two pieces,
   `[pos1, len]` and `[1, pos2]`, joined by a wrap glyph at the origin rule; it
   occupies one lane. Partial ends are drawn with an open (dashed) edge on the
   partial side. Joined exon pieces share a colour and a thin connector.
   The selected feature has a 2 px outline in `--mp-primary`.
3. **Nucleotide row.** Shown only when the Nucleotides box is on. At
   `pxPerBase >= 8`: one letter per base, base-coloured tiles (A green, C blue,
   G orange, T red, N grey, the same values the MapToRef pileup uses). At
   `3 <= pxPerBase < 8`: coloured bars, no letters. Below 3: hidden (the ruler
   and lanes remain).
4. **Amino-acid rows.** Shown only when the Amino acids box is on and
   `pxPerBase >= 4`. One row per PCG that overlaps the view, labelled with the
   gene name in the left gutter, in lane order. Codon `i` (0-based) of a plus
   strand gene covers bases `pos1 + 3i .. pos1 + 3i + 2`; of a minus strand gene
   `pos2 - 3i - 2 .. pos2 - 3i`; all positions taken modulo `len` on a circular
   unit (wrap-around genes are contiguous in codon space, matching how the app
   splices them before translating). The letter `translation[i]` is drawn
   centred on the codon's middle base; a codon past the end of the translation
   draws "*" when it is the stop codon (`pos2` side for plus, `pos1` side for
   minus) and nothing otherwise. Letters use the app's monospace token and the
   same size as nucleotide letters; hydrophobic/polar shading is not drawn
   (keep it readable, not busy). Partial genes start their codon index at the
   annotated start; a `partial5` gene therefore shows the stored translation's
   frame exactly as the app computed it.

Semantic zoom means the whole genome fits in the canvas width (arrows only, no
letters) and the deepest zoom shows 14 px per base.

## 6. Navigation

- Wheel or pinch: zoom about the pointer. Drag: pan. Shift-wheel: pan.
- Zoom buttons: factor 2 about the view centre. `Whole genome`: `pxPerBase =
  width / len`, `viewStart = 1`. `Fit gene`: the selected feature's span plus 5%
  margin, capped at 14 px per base. `Position:` centres on that base at the
  current zoom (or 10 px per base if the current zoom hides letters).
- Circular units: the view may span the origin; drawing walks positions modulo
  `len`, so a view of `[len - 300, 300]` shows the seam with the origin rule in
  the middle. Linear units clamp the view to `[1, len]`.
- Hover: a tooltip (`.mp-maptoref-tip` styling, shared) with the position, the
  base, the gene under the pointer (lane hit-test), and for an amino-acid row
  the codon index and letter.
- Click on a gene arrow: sets Shiny input `seqview_pick` to the feature's `row`;
  the R side selects that table row and scrolls the table to it. Click on empty
  space clears the selection.

## 7. Jump from the annotation table

When the table selection changes to exactly one row (the existing
`getReactableState("table", "selected")` reactive), the module sends a
`mpseq_select` message with the row index; the viewer opens the section if it is
closed (native `<details>` open, as the MapToRef click does for its pileup),
selects the feature, and fits it. Deleted rows (`pos1 == 0`) are ignored.

## 8. Keeping up with edits

The payload observer depends on: `rv$annotations` (every edit path assigns this
field, including the in-memory nudges of the alignment edit mode, so a nudge
moves the arrow and the amino-acid letters before Save), `asmb_edit_tick()`
(trim, restore, linearize bump it), `rv$updating$topology` (linearize flips it),
the modal-open nonce (reopen reloads from the database), and the edit session's
sequence (`rv$editing$assembly`, set on Edit and cleared on Save/Discard).

The browser compares `version` and `len`: if unchanged, it keeps the viewport and
selection and only replaces the feature list; otherwise it refits to the whole
genome. Sending the whole payload every time is deliberate: 20 KB per edit is
cheaper than a diff protocol and cannot drift.

Deleted features are soft-deleted in the app (`pos1 = pos2 = 0`, gene renamed
with a `_DELETED_` suffix); the R side drops them from the payload so they never
draw. Restore brings them back through the same observer.

## 9. Files

- `inst/app/www/seqviewer.js`: the viewer (about 500 to 700 lines): state,
  scale, lane packing, codon geometry, draw, hit-test, tooltip, message handlers
  (`mpseq`, `mpseq_select`), input events (`seqview_pick`, `seqview_view` for
  the harness). Exposes `window.mpseq.state(id)` for tests. Registered in
  `add_external_resources()` next to `custom.js`.
- `inst/app/www/custom.css`: a block "Sequence viewer" with the section, control
  row, canvas wrapper, and gutter rules on the tokens.
- `R/app_annotate_seqview.R`: `seqview_ui(id)` (the section with controls and
  the canvas placeholder) and `seqview_server(id, rv, tick, selected)` (payload
  builder `seqview_payload()`, the observers, the pick handler). The payload
  builder is a pure function of (annotations frame, sequence, topology,
  selected) so it can be unit-tested without Shiny.
- `R/app_annotate_details.R`: mount the UI in `annotate_details_modal()` and
  call `seqview_server()` from `annotations_details_server()` with the existing
  `rv`, `asmb_edit_tick`, and the table-selection reactive; select the table row
  on `seqview_pick`.
- `tests/testthat/test-seqview-payload.R`: codon geometry on both strands, a
  wrap-around gene, a partial gene, deleted rows dropped, a joined gene's marker.
- `dev/ui_review/steps/seqview_*.R`: harness scripts that open the window, jump
  to a gene, nudge a codon in edit mode, delete and restore a feature, linearize,
  and read `window.mpseq.state()` after each to assert the viewer followed.

## 10. Error handling

- No sequence (unit not in `assemblies`): the section shows "No sequence stored
  for this assembly." and no canvas.
- No features: the canvas draws the ruler and sequence only.
- A translation shorter than the gene implies: letters stop where the string
  ends; nothing is invented.
- Payload send failures are impossible to observe from R; the JS logs to the
  console and keeps the last good state.

## 11. Performance

Whole-genome draw: 40 arrows and a ruler, well under a frame. Letter zoom: at
most `width / 8` bases drawn, about 200 letters per row, a few rows. Payload
build in R: one `get_assembly()` per sequence change (not per annotation edit,
because the sequence is cached in the module until `version` changes). No
`bindCache` needed.

## 12. Not changed

The per-row nt/aa copy buttons stay; the nt button's missing reverse-complement
for minus-strand genes is a separate fix. The Alignment section keeps msaR. The
Coverage Map keeps its gggenes arrows (the two arrow styles will differ slightly;
acceptable, the map is about depth, the viewer about sequence).
