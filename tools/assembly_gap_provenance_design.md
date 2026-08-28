# Assembly gap provenance and linkage evidence

Date: 2026-08-28
Status: approved for implementation

## Problem

Export declares runs of `N` as `assembly_gap` features, but it cannot tell where
a run came from, so every gap is written with the same qualifiers:

```
estimated_length  <run length>
gap_type          within scaffold
linkage_evidence  unspecified
```

Two of those are wrong in some cases.

1. **`estimated_length`.** `join_scaffolds()` sizes a junction two different
   ways. When the reference places the neighbouring scaffolds apart by *n* bp it
   inserts *n* Ns, which is a genuine estimate. When the junction is unmapped it
   inserts `gap_len_default` (100) Ns, which is a placeholder and matches the
   GenBank/EMBL/DDBJ convention for a gap of unknown size. Export reports both as
   a measured length, so a placeholder is submitted as a 100 bp measurement.

2. **`linkage_evidence`.** The AGP specification reserves `unspecified` for
   contamination gaps and for converting old AGPs that lack the field. A gap we
   created by ordering scaffolds against a reference mitogenome has real
   evidence: `align_genus` or `align_xgenus`, depending on whether that reference
   shares the sample's genus. Ns that were already present in a sequence the user
   supplied have no evidence we can vouch for, and are the one case where
   `unspecified` is defensible.

Deciding `align_genus` vs `align_xgenus` needs the sample's genus, which
MitoPilot does not hold in controlled form: `samples.Taxon` is free text, and
`assemble.blast_lineage` stops at family (e.g. `Chordata; Anguilliformes;
Congridae`). Only the user can settle it.

## Design

### 1. Persist what the join did

`join_scaffolds()` already builds a per-junction record (`junc_rec`) carrying
`from`, `to`, `type`, `gap_bases`, `overlap_len` and `identity`, then discards
it. Persist it.

New table, written by the scaffold-join workflow alongside `scaffold_mappings`:

```sql
CREATE TABLE scaffold_junctions (
  ID           TEXT NOT NULL,
  junction     INTEGER NOT NULL,   -- 1-based order along the joined sequence
  from_scaffold TEXT,
  to_scaffold   TEXT,
  type         TEXT,               -- 'gap' | 'butt' | 'overlap'
  gap_bases    INTEGER,            -- Ns inserted, 0 for butt/overlap
  size_known   INTEGER,            -- 1 = reference estimate, 0 = placeholder
  time_stamp   INTEGER,
  PRIMARY KEY (ID, junction)
)
```

`size_known` is the distinction export needs: 1 when the length came from the
reference alignment, 0 when the junction was unmapped and took
`gap_len_default`.

**Positions are deliberately not stored.** Circularization trimming and
`rotate_to_reference()` both reindex the joined sequence after the join and carry
only the coverage vectors, so any position recorded at join time can be stale.
Export therefore locates runs by scanning the final sequence, which is always
correct, and uses the table only to classify what it finds.

### 2. Record the user's genus call

```sql
CREATE TABLE gap_evidence (
  ID          TEXT NOT NULL,
  genus_match TEXT,          -- 'same' | 'different'
  time_stamp  INTEGER,
  PRIMARY KEY (ID)
)
```

Keyed by sample, not by export unit, so the choice survives regrouping,
re-export, and changes to which units exist.

### 3. Ask for it in the app

A new pre-flight gate in the export flow, after the existing invalid-header,
multi-path and fragmented-sample checks and before `run_export()`. It appears
only when samples in the group actually contain gaps.

One row per sample: **Sample ID**, **Taxon**, **reference accession and
species**, **gaps found** (count and total bp), and a **same / different**
toggle for "reference vs sample genus". The toggle is pre-filled by comparing the
first whitespace token of `Taxon` with the first token of `blast_species`, both
of which are the genus when the values are conventional binomials; the user is
correcting a suggestion rather than filling a blank form. Confirming writes every
row to `gap_evidence` and continues the export.

### 4. Classify each run on export

For every run of `N` at or above `gap_min` in a unit being exported:

| Unit | Run matches an unmapped junction's length | `estimated_length` | `linkage_evidence` |
|---|---|---|---|
| joined | yes | `unknown` | `align_genus` / `align_xgenus` |
| joined | no | run length | `align_genus` / `align_xgenus` |
| not joined | n/a | run length | `unspecified` |

`align_genus` when the sample's `gap_evidence.genus_match` is `same`,
`align_xgenus` when `different`. With no stored choice, which is what a headless
`export_files()` call gets, evidence falls back to `unspecified` and this is
documented.

A unit counts as joined when it has rows in `scaffold_junctions`. Matching an
unmapped junction by length is exact except when a reference estimate happens to
equal `gap_len_default` (100) to the base, where an estimated gap is reported as
unknown-size. That is the conservative direction and is documented.

## Out of scope

- Changing how the join sizes gaps. The reference estimate is real information
  and preserving reading frame across a gap depends on it.
- Controlled taxonomy for `samples.Taxon`.

## Testing

Unit tests over simulated joined assemblies rather than pipeline runs: build
scaffold sets with known gaps, join them, and assert the persisted junctions, the
classification of each run, and the emitted qualifier blocks. Cover a
reference-estimated gap, an unmapped placeholder gap, a butt join, a
user-supplied sequence with internal Ns, both genus answers, and the no-choice
fallback.
