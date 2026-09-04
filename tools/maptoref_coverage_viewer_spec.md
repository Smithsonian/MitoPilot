# MapToRef Coverage and Pileup Viewer

Design spec, 2026-09-04. Branch `map-to-ref-assembly`.

## 1. Purpose

Give users an interactive, in-app view of how their reads map to the MapToRef
reference: a zoomable coverage plot across the whole reference, an annotation
track when the reference is a GenBank record, and a click-through read pileup
for inspecting individual positions.

Today a MapToRef sample offers only a static coverage PDF opened in an external
viewer (`R/app_assemble_coverage_details.R:444-463`). There is no way to see the
reads themselves, and no way to relate a coverage dip to the gene it falls in.

## 2. Scope

In scope:

- A new detail modal for MapToRef samples, reached from the Assemble table.
- Coverage track across the full reference, with zoom and hover.
- Annotation track from the reference GenBank record.
- Read pileup panel for a user-chosen window of 50 to 1000 bp.
- The small pipeline changes needed to keep the data these need.

Out of scope:

- Any change to the shared `coverage()` step or its outputs.
- Viewers for the GetOrganelle or MitoFinder assemblers.
- Editing, curation, or variant calling from within the viewer. It reads only.
- Any database schema change.

## 3. The reference coordinate frame

Everything in this feature lives in one coordinate system: reference positions
`1..reference_length`.

This is possible because the MapToRef iteration calls `samtools consensus` with
`--show-ins no` (`R/map_to_ref.R:590`), so no consensus produced by the loop
ever changes length. The converged reference `ref_final.fa` therefore has the
same coordinates as the original reference, and `final.bam`
(`R/map_to_ref.R:650-659`) is the full read set mapped against it. GenBank
feature coordinates apply directly, with no liftover.

Four tracks fall out of that single frame:

| Track | Source file | Status today |
|---|---|---|
| reference bases | `maptoref/ref.fasta` | already written |
| consensus bases | `maptoref/subs_only.fasta` | already written |
| reads | `maptoref/final.bam` | built, then deleted |
| annotations | `maptoref/reference.gb` | written on the NCBI fetch path |

Note the contrast with the shared coverage step. `R/coverage.R` maps reads to
the *published* assembly, which has been indel-spliced and, for a linear
publication, end-trimmed. Its BAM and `_coverage.csv` are therefore in published
coordinates, not reference coordinates, and cannot carry the annotation track.
This feature deliberately does not reuse them.

### 3.1 The circular seam

For a circular reference, `ref_final.fa` carries a `flank`-length copy of the
start appended to the end. Alignments landing in that tail have positions past
`reference_length` and must be folded back onto `position - reference_length`,
summing depth at the shared position. This is the same operation as
`.coverage_reform_circular()` (`R/coverage.R:265`) and should reuse that
function's logic rather than restate it.

## 4. Pipeline changes

Four changes, all in `R/map_to_ref.R` except where noted.

1. **Keep the BAM.** The cleanup at `R/map_to_ref.R:767-769` currently unlinks
   every `.bam` and `.bai` in the work directory on success. Narrow that pattern
   so `final.bam` and `final.bam.bai` survive. All other transients keep being
   removed.

2. **Index unconditionally.** `samtools index` currently runs only for circular
   references, to serve the junction-depth query (`R/map_to_ref.R:663-665`).
   Move it so it always runs after `final.bam` is sorted.

3. **Write `maptoref/maptoref_depth.csv`.** Columns `Position,Depth`. Produced
   with `samtools depth -a -J final.bam`, seam-folded per section 3.1, and
   trimmed to `1..reference_length`. About 16,000 rows, a few hundred KB. This
   is what the coverage track reads, so the whole-reference view never has to
   open the BAM.

4. **Write `maptoref/maptoref_features.csv`.** Columns
   `type,gene,start,end,strand`. Parsed once from the reference GenBank record.
   Written only when a `.gb` is present, which is the NCBI-efetch path
   (`R/map_to_ref_refs.R:483-485`) or a user-supplied `.gb`. A reference that
   came from the local BLAST database or a user FASTA has no annotations, and
   no file is written.

`inst/nextflow/modules/assemble.nf:20` already declares the whole `maptoref/`
directory as an optional output, so no Nextflow output declaration changes.

### 4.1 Disk cost

Retaining `final.bam` is a permanent per-sample cost, roughly the size of the
mapped read set: tens to hundreds of MB depending on sequencing depth. This is
accepted deliberately. If it becomes a problem, capping retained depth by
subsampling is a self-contained follow-up, not a redesign.

## 5. New files

### 5.1 `R/maptoref_features.R`

Pipeline-side. Converts a GenBank record to the features data.frame.

- `maptoref_parse_features(gb_path)` returns a data.frame with
  `type,gene,start,end,strand`.
- Keeps `CDS`, `tRNA`, `rRNA`, and `D-loop` features. Drops `source` and
  `rep_origin`. A bare `gene` feature is kept only when no typed feature shares
  its exact span: mitogenome records annotate nearly every gene twice, once as
  `gene` and once as its type, and drawing both would double every arrow.
  Verified against `inst/test_data/NC_002333_Danio_rerio.gb`, which reduces from
  77 raw features to the expected 38.
- Resolves `complement(...)` to `strand = "-"`.
- For `join(a..b,c..d)` spans, emits one row per segment, all sharing the gene
  name, so a split gene draws as two arrows rather than one arrow spanning the
  gap.
- Gene naming falls back through `/gene`, then `/product`, then `/locus_tag`,
  then the feature type, so a row is never nameless.

Parsing uses `read.gb` (CRAN, v2.2, July 2025). `genbankr` is deliberately not
used: it has been removed from Bioconductor and is not installable from a
current release.

### 5.2 `R/maptoref_viz_data.R`

The non-reactive data layer. No Shiny, fully testable.

- `maptoref_paths(dir_out, ID, assemble_opts)` returns a named list of the
  file paths this feature uses: reference FASTA, consensus FASTA, BAM, BAM
  index, GenBank record, depth CSV, features CSV, and summary text. The path convention
  `file.path(dir_out, ID, "assemble", assemble_opts, ...)` is currently rebuilt
  inline in several places in `R/app_assemble_coverage_details.R`; this
  centralizes it for the new code.
- `maptoref_read_depth(path)` reads the depth CSV.
- `maptoref_read_features(path)` reads the features CSV, returning an empty
  data.frame when absent.
- `maptoref_bin_depth(depth, n = 2000)` bins a depth series to at most `n`
  points using the maximum within each bin, so spikes and dropouts survive
  downsampling. Used for wide views only.
- `maptoref_window_reads(bam, seqname, start, end, ref_seq, max_reads = 100)`
  fetches one window and returns stacked, mismatch-annotated read rows. See
  section 7.

### 5.3 `R/app_assemble_maptoref_viewer.R`

The Shiny module: `maptoref_viewer_ui()` and `maptoref_viewer_server(id, rv)`.

### 5.4 Tests

`tests/testthat/test-maptoref-features.R` and
`tests/testthat/test-maptoref-viz-data.R`. See section 10.

## 6. User interface

### 6.1 Entry point

A Detail button on MapToRef rows in the Assemble table, wired exactly as the
existing coverage modal is: the button's `colDef` cell renders through
`rt_icon_bttn_text()`, its `observeEvent` sets `rv$updating` to the clicked row
and fires a gargoyle trigger, and the module listens with `on(...)` and calls
`showModal()`. This mirrors `R/app_assemble.R:447-456`,
`R/app_assemble.R:1352-1357`, and `R/app_assemble_coverage_details.R:57-194`.

The button appears only when the row's assembler is `MapToRef`.

### 6.2 Layout

A wide modal. At the top, a header strip with the sample ID and, read from
`_summary.txt`, the organism, accession, reference length, reference source,
reads mapped, and percent N (`n_count` over `reference_length`). Mean depth is
not in the summary file and is computed from the depth CSV.

Beside it, a reference picker. A sample carries a single `maptoref_ref` value
today, so this will usually hold one entry; it lists the reference and
option-set combinations actually found on disk via `assemble_dirs_on_disk()`
(`R/project_consistency.R:25-33`), and collapses to a plain text label when
there is only one. Changing the selection redraws every track.

Below that, three vertically stacked panels sharing an x axis, assembled with
`patchwork` using vertical alignment:

**Annotation track.** Gene arrows drawn with `gggenes`, already an import.
Colored by feature type. Gene labels drawn only when the visible window is
narrow enough for them not to collide. When the reference has no annotations,
this panel is replaced by a one-line note reading that the reference has no
annotation record.

**Coverage track.** The main panel. Read depth as a filled histogram. Regions
of zero depth marked in `#FF6670`, the same color the existing coverage plot
uses for its outlier mask (`R/coverage.R:200`).

**Pileup panel.** Hidden until the user clicks the coverage track. See section 7.

### 6.3 Zoom and hover

Viewer state is a window center and a window size in bp.

- Fully zoomed out is the entire reference.
- Fully zoomed in is 100 bp.
- Controls are a `-` and `+` pair stepping by a factor of two, a numeric window
  size box, and a Full view reset.
- Dragging a selection across the coverage panel zooms to that selection.

Hover uses `plotOutput(hover = hoverOpts(delay = 100, delayType = "throttle"))`
with an absolutely positioned tooltip element showing the reference position,
the depth at that position, and the gene under the cursor when there is one.
This is a stock Shiny pattern and adds no dependency.

When the visible window is wider than 2000 bp the depth series is binned per
`maptoref_bin_depth()` before plotting, so the whole-reference view draws about
2000 points rather than 16,000.

## 7. Pileup panel

Clicking the coverage track opens the pileup centered on the clicked base. The
panel has its own window size box, default 200 bp, range 50 to 1000, kept
independent of the main plot's zoom level.

### 7.1 Fetching

Reads are fetched with `Rsamtools::scanBam` under a `ScanBamParam` whose `which`
is restricted to the visible window, so only that slice is ever read. The
requested fields are `pos`, `qwidth`, `cigar`, `seq`, `strand`, `flag`, and
`mapq`. Passing an explicit region is essential: without it Rsamtools scans the
entire file.

`which` is given as an `IRangesList` rather than a `GRanges`, which keeps
`GenomicRanges` off the dependency list.

If `final.bam.bai` is missing, which can happen for a sample assembled before
this feature landed, it is built on demand with `Rsamtools::indexBam` and left
in place for later opens.

### 7.2 Mismatch and indel derivation

Mismatches are computed by walking each read's CIGAR and comparing the aligned
read base to the displayed reference base, rather than by reading MD tags. Two
reasons: MD tags are relative to the *converged* reference, whereas the panel
displays the *original* reference, and the CIGAR walk is needed for indels
regardless, so this derives everything in one pass.

The walk handles `M`, `=`, `X`, `I`, `D`, `N`, `S`, and `H`, and yields per
read: a list of reference-position and base pairs for aligned bases, deletion
intervals, and insertion positions with their lengths.

### 7.3 Stacking

Reads are assigned to display rows by greedy interval packing: each read goes
into the first row whose last read ends before it starts, with a small gap. At
most `max_reads` rows are drawn, default 100. When more reads overlap the
window, the panel shows a note reading, for example, "showing 100 of 3,412
reads", so a very deep pileup cannot stall rendering.

### 7.4 Rendering

Drawn with ggplot2, consistent with the rest of the codebase.

- Each read is a `geom_rect`, filled by strand in two muted colors.
- Mismatched bases are drawn as `geom_text` letters colored by nucleotide.
- Deletions are a thin `geom_segment` bridging the gap.
- Insertions are a narrow tick mark drawn between the flanking bases.
- Above the reads sit two rows of tiles: the reference bases and the consensus
  bases from `subs_only.fasta`. Base letters are drawn when the window is under
  roughly 300 bp; above that the rows render as colored tiles only.

## 8. Dependencies

Added to `Imports`:

- `Rsamtools`, Bioconductor. Reads the BAM. MitoPilot already imports
  `Biostrings`, `DECIPHER`, and `BiocGenerics`, so the Bioconductor chain is
  established.
- `IRanges`, Bioconductor. Supplies the region for `ScanBamParam`.
- `read.gb`, CRAN. GenBank parsing.

Explicitly not added: `plotly`, `igvShiny`, `JBrowseR`, `GenomicRanges`,
`ggcoverage` (archived from CRAN in November 2023), `genbankr` (removed from
Bioconductor).

No JavaScript is introduced. All rendering is server-side ggplot2 through
`renderPlot`, matching the existing architecture.

## 9. Failure modes

Each of these degrades to a working subset rather than an error page.

| Condition | Behavior |
|---|---|
| No `final.bam`, e.g. a sample assembled before this change | Coverage and annotation tracks work from the CSVs. Pileup panel shows a short note that the sample must be re-run to enable the read view. |
| No `maptoref_depth.csv` | A sweet-alert in the style of `require_assemble_output()` (`R/app_assemble_coverage_details.R:18-51`), naming what is on disk. |
| No `.gb` for the reference | Annotation track hidden, replaced by a one-line note. |
| Missing `.bai` | Rebuilt on demand with `Rsamtools::indexBam`. |
| Window contains no reads | Pileup panel renders empty axes with a "no reads in this window" note. |
| Failed MapToRef run | The Detail button is not offered, matching how the existing detail button is gated on assembly having succeeded. |

## 10. Testing

The data layer carries the tests, since it holds all the logic worth testing.

Test BAMs are built inside the tests: write a small SAM by hand, convert with
`Rsamtools::asBam`, and index it. This needs no external samtools binary, so the
tests run anywhere the package installs.

Cases:

- CIGAR walking against hand-checked expectations, covering a clean match, a
  substitution, an insertion, a deletion, and soft clipping.
- Read stacking: non-overlapping reads share a row, overlapping reads do not,
  and the `max_reads` cap truncates while reporting the true total.
- Seam folding: a circular reference with alignments in the flank tail produces
  depth summed onto the correct low-numbered positions.
- Depth binning: spikes survive downsampling, and the output length respects the
  requested point count.
- GenBank parsing against a trimmed real record: strand resolution, `join(...)`
  spans producing one row per segment, and the gene-name fallback chain.

## 11. Backwards compatibility

No database schema change and no migration in `R/backwards_compatibility.R`.
Every artifact this feature uses is a file on disk, discovered by the existing
path convention.

Existing projects keep working. Samples assembled before this change have no
`final.bam` and no depth CSV; they fall into the degraded paths in section 9
and become fully viewable after a re-run.

## 12. Open items

None blocking. The disk-cost mitigation in section 4.1 is a known future option
rather than an unresolved question.
