# NCBI organelle triage + INSDC minimal standards vs. MitoPilot

Gap analysis of MitoPilot's mitochondrial curation / validation / export against
(1) the NCBI GenBank organelle review team's internal triage protocol and (2) the
authoritative INSDC Minimal Specifications. Scope: **animal mitochondria** (plant
/ fungal / chloroplast portions are out of scope).

## Sources

- **NCBI internal organelle triage protocol** (`internalorgtriage.docx`). The
  team's own accept/reject checklist. Mixes true INSDC minimal standards with
  stricter internal heuristics.
- **INSDC Minimal Specifications v1.0** (adopted by NCBI March 2026). The
  authoritative baseline. Read directly:
  - Annotation Minimum Specification v1.0 and Sequence minimum standards v1.0,
    `https://github.com/INSDC-Repo/INSDC` (`INSDC Minimal Specifications/`).
  - Landing pages: `https://www.insdc.org/insdc-minimal-specifications/`,
    `https://www.ncbi.nlm.nih.gov/genbank/collab/insdc_min_spec/`.
- NCBI organelle submission guide:
  `https://www.ncbi.nlm.nih.gov/genbank/organelle_submit/`.

### Critical distinction

The triage doc does not separate INSDC *minimal* standards from the review
team's stricter internal guidance. Verified against the spec:

- **tRNA 50-150 bp IS an INSDC minimal standard.** Annotation spec, Feature
  lengths #2: "Complete tRNA features must be between 50 and 150 bps long."
- **rRNA >= 800 (12S) / >= 1000 (16S) is NOT an INSDC standard.** The INSDC spec
  sets no rRNA length threshold at all. Those numbers are internal
  organelle-triage heuristics only.

Throughout this document each item is tagged **[INSDC]** (a true minimal
standard) or **[triage]** (internal heuristic).

---

## 1. INSDC Annotation minimal standards vs. MitoPilot coverage

| INSDC rule (spec section) | MitoPilot status |
|---|---|
| Complete tRNA 50-150 bp (lengths #2) **[INSDC]** | **GAP** - `tRNA$min_len`/`max_len` are `NA` in every `params_*_mito.R` (e.g. `R/params_fish_mito.R:27-31`) |
| Complete CDS >= 30 aa (lengths #1) **[INSDC]** | **GAP (minor)** - not explicitly checked; all 13 metazoan PCGs exceed this |
| Introns >= 10 bp (lengths #3) **[INSDC]** | **GAP (minor)** - not checked; intron genes handled by exon-join |
| No internal stop codons, accounting for transl_except (seq-dep #ii) **[INSDC]** | COVERED - `R/validate_mito_core.R:259-262` (`internal stop codon`) |
| Complete CDS starts with valid start codon unless 5' partial (coding #2) **[INSDC]** | COVERED - `R/validate_mito_core.R:270-274` (`non-standard start codon`) + `partial_start` |
| Complete CDS ends with valid stop codon unless 3' partial (coding #3) **[INSDC]** | COVERED - `R/validate_mito_core.R:264-268` (`non-standard stop codon`) + `partial_stop` |
| Partiality only marks feature *longer*; internal boundaries not partial; `<100..>200` not `100..<200` (location #5) **[INSDC]** | COVERED - `partial_start`/`partial_stop` written as `<`/`>` on the outer coordinates only, `R/export.R:659-661, 679-683` |
| transl_except = one full codon (or 1-2 bp stop completed by polyadenylation) (coding #5) **[INSDC]** | COVERED - `R/export.R:663-678` (`transl_except (pos:...,aa:TERM)` + poly-A note) |
| Translation must match conceptual translation; IUPAC only (seq-dep #ii-iv) **[INSDC]** | Partly - `Biostrings::translate` enforces conceptual translation; IUPAC/N-in-CDS is a known edge (see `mitos-ambiguity-crash` note) |
| Ribosomal RNA features must not overlap CDS or other rRNAs (overlaps #2) **[INSDC]** | **GAP** - only the generic `max_overlap` fraction check (`R/validate_mito_core.R:150-201`); no dedicated rRNA-overlap rule |
| tRNA features must not be fully contained within a CDS exon (overlaps #3) **[INSDC]** | COVERED (stricter) - `tRNA within PCG or rRNA`, `R/validate_mito_core.R:203-215` |
| Location descriptors + strand for all features (location #1) **[INSDC]** | COVERED by the annotation data model |
| Each gene feature has a unique locus_tag (locus tags) **[INSDC]** | Downstream - locus tags assigned at submission, not by MitoPilot |
| CDS/tRNA overlap only a few nt (metazoan mito) **[triage]** | COVERED - per-gene start/stop `overlap` rules + `max_overlap` |
| Truncated / over-extended proteins vs. BLAST + multi-alignment **[triage]** | COVERED - `low reference similarity` / `check reference start\|stop alignment` (`R/validate_mito_core.R:276-291`) + PCG outlier MSA review at export (`flag_PCG_outliers`, `R/export.R`; `inst/AA_alignment_report.Rmd`) |
| Duplicate features **[triage]** | COVERED - `possible duplicate` via per-clade `count` rules (`R/validate_mito_core.R:105-110`) |
| Expected 13 CDS / 22 tRNA / 2 rRNA **[triage]** | COVERED - `R/constants.R:7-25` + per-clade `count` |

Feature-vs-moltype rules (tRNA not on rRNA sequence, CDS not on rRNA, etc.) and
the ribosomal-slippage / trans-splicing exception table in the Annotation spec
are satisfied by MitoPilot's data model and its JOIN / `ribosomal_slippage`
export path (`R/export.R:494-504`, `R/app_annotate_details.R`), which also covers
the bird/turtle ND3 and Aprion CYTB frameshift cases from the triage doc.

---

## 2. Recommendation A - length + overlap checks

MitoPilot's validation engine already emits `below min length` /
`exceeds max length` whenever a gene rule sets `min_len`/`max_len`
(`R/validate_mito_core.R:225-232`). The gaps are unset thresholds, not missing
logic. Ranked by how well-grounded each is:

1. **tRNA 50-150 bp - [INSDC], strongly recommended.** Set
   `tRNA$min_len = 50, tRNA$max_len = 150` in the `default_rules` block of the
   `params_*_mito.R` files (pattern at `R/params_fish_mito.R:27-31`). Rules merge
   via `modifyList`, so this is a per-clade default edit with **no engine
   change**; the existing `below/exceeds length` warnings fire automatically.
   Note: this bound applies to tRNAs that are *present*; clades with reduced
   tRNA sets (Cnidaria, Ctenophora) already handle absence through `count`.

2. **rRNA must not overlap CDS / other rRNA - [INSDC].** Add a dedicated check in
   `validate_mito_core.R` mirroring the existing tRNA-in-PCG block
   (`R/validate_mito_core.R:203-215`): for each rRNA, flag any overlap with a CDS
   or another rRNA. This is a small engine addition (one new warning string),
   listed as optional if a zero-engine-change pass is preferred.

3. **CDS >= 30 aa and introns >= 10 bp - [INSDC], low practical impact.** All 13
   metazoan PCGs already exceed 30 aa, so this is a cheap guard rather than a
   real-world catch. Could be a per-clade PCG `min_len` (~90 bp) and an intron
   length check. Low priority.

4. **rRNA >= 800 (12S) / >= 1000 (16S) - [triage], NOT INSDC.** If added at all,
   present it as an *optional* per-clade sanity warning explicitly labeled
   non-INSDC. Do **not** apply it to clades with legitimately short / fragmented
   rRNAs: Ctenophora (short rRNAs), Porifera Calcarea and Hexactinellida
   (fragmented rRNAs). Current rRNA rules set only `max_len` (e.g.
   `R/params_fish_mito.R:39-46`); any minimum must be per-clade, never a global
   constant.

---

## 3. Recommendation C - taxon notes in the ruleset browser

`R/ruleset_browser.R` builds a self-contained HTML browser from `RULESET_MAP`
(`R/ruleset_browser.R:3`) plus each clade's params. It shows genetic code,
global params, and per-gene rule tables, but **no free-text biological
guidance**. The triage doc's lineage-specific notes are exactly that kind of
guidance and belong here.

**Approach:** add a per-clade notes list (a new `RULESET_NOTES` keyed by target,
or a `notes` field on each `RULESET_MAP` entry), then render a "Clade notes"
panel in `build_ruleset_display()` (`R/ruleset_browser.R:334`) and
`selectRuleset()` (`R/ruleset_browser.R:742`), above the rule tables. Keep R
sources ASCII-only (use the existing glyph-token convention,
`R/ruleset_browser.R:456-466`). Tag each note **[INSDC]** or **[triage]**.

**Triage-note -> clade-target mapping** (targets verified present in
`RULESET_MAP`):

| Target | Note (source) |
|---|---|
| `diptera_mito` | 5' partial COI common (often starts ~RWQ...); ND5 usually uses a truncated TAA stop before tRNA-Phe. **[triage]** |
| `scyphozoa_mito`, `hydrozoa_mito` | Medusozoan mtDNA may be linear and/or split into 2/4/8 fragments; reduced tRNA set (Met, sometimes Trp); parts of COI repeated. **[triage]** |
| `hexacoral_mito`, `octocoral_mito` | Anthozoans typically a single tRNA-Met; multi-exon ND5 / COX1 in some cnidarians. **[triage]** |
| `bivalvia_mito`, `gastropoda_mito` | Doubly-uniparental inheritance: male and female genomes differ in size; may carry extra M-ORF / F-ORF. **[triage]** |
| `bird_mito`, `turtle_mito` | ND3 has an extra base skipped during translation (Mindell et al. 2003, PMID 12572620); annotate as a join with `ribosomal_slippage`. ATP6 sometimes lacks a start codon - mark 5' partial if the size is correct. **[triage]** |
| `ctenophore_mito` | Highly compact; lacks ATP6 and ATP8; short rRNAs; no tRNAs; TGA reassigned from Trp to Ser in some species. **[triage]** |
| `demospongiae_mito`, `homoscleromorpha_mito` | Highly variable gene order / content / boundaries; cox1 introns in some taxa. **[triage]** |
| `platyhelminthes_mito`, `nemertea_mito` | tRNAs may all sit on one strand (trematodes all plus-strand). May lack ATP8. **[triage]** |
| `malacostraca_mito`, `copepod_mito`, `annelid_mito` | Invertebrate mito code (transl_table 5); ATP8 present but short. **[triage]** |
| All metazoan clades | Complete tRNAs must be 50-150 bp; rRNAs must not overlap CDS or other rRNAs; complete CDS >= 30 aa. **[INSDC]** |

---

## 4. Sequence-level INSDC standards (context)

From the Sequence minimum standards v1.0 - mostly enforced downstream or at
submission, but relevant background for MitoPilot's export path:

- Minimum sequence size 100 nt, with biological exceptions for complete tRNA /
  microRNA / patent / ancient-DNA. **[INSDC]**
- No more than 50% Ns unless the sequence is a complete chromosome. **[INSDC]**
- IUPAC characters only; T not U regardless of moltype. **[INSDC]**
- No vector / linker unless labeled synthetic; screen for contamination
  (UniVec / FCS). **[INSDC]** - MitoPilot does not screen; NCBI/INSDC run this at
  submission. (The triage "vector/Illumina primer" stop maps here.)
- Multi-chromosome organelle naming: chromosome / fragment name <= 33 chars,
  cannot include "chr" / "chromosome" / "contig" / "scaffold" / genus / species;
  names are included only when the organelle is known to have multiple
  independent chromosomes. **[INSDC]** - directly relevant to MitoPilot's
  multipartite / multi-scaffold export path; worth honoring when naming
  fragments.

---

## 5. Considered but not recommended now

- **Per-clade gene-presence audit.** Relax the remaining hard `atp8 count = 1`
  in invertebrate clades the triage doc flags as ATP8-optional (several inverts
  already use `count = c(0, 1)`, e.g. `params_bivalvia_mito.R`,
  `params_ascidiacea_mito.R`); verify ctenophore/cnidarian reduced gene + tRNA
  content. Deferred - a params review, not a standards gap.
- **tRNA strand-distribution check.** Triage: tRNAs expected on both strands,
  with exceptions (Nematoda, Platyhelminthes, Mollusca, Tunicata, trematodes).
  Would need a new `validate_mito_core.R` check plus a per-clade
  `expect_both_strands` param. Deferred - engine change, and it is triage
  guidance, not an INSDC standard.
- **Vector / primer contamination screen.** Needs UniVec / VecScreen / FCS. Out
  of scope for the annotation engine; INSDC/NCBI run it at submission.

## 6. Out of scope

Plant / fungal / chloroplast multipartite and trans-splicing rules, `rps12`,
chloroplast tRNA-Ile, RNA-editing pseudogene handling; cross-submission
duplicate detection (an NCBI-side check with no local analog).

---

## Summary

- MitoPilot already satisfies most INSDC annotation minimal standards relevant to
  animal mito: internal stop codons, valid/partial start-stop codons, partiality
  direction, tRNA-not-contained-in-CDS, translation-exception form, expected gene
  set, overlap caps, and BLAST/multi-alignment truncation review.
- The clearest, best-grounded gap is the **INSDC tRNA 50-150 bp** length bound,
  closeable with a per-clade default edit and no engine change.
- A dedicated **rRNA-must-not-overlap-CDS/other-rRNA** check (INSDC) and the
  minor CDS >= 30 aa / intron >= 10 bp checks are worth adding but lower value.
- The **rRNA 800/1000 minimum** from the triage doc is NOT an INSDC standard and
  should only ever be an optional, clade-aware, clearly-labeled heuristic.
- The ruleset browser is the right home for the triage doc's clade-specific
  biological guidance, each note tagged INSDC-standard vs triage-heuristic.
