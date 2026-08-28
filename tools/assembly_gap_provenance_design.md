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
   evidence: `align-genus` or `align-xgenus`, depending on whether that reference
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
  junction     INTEGER NOT NULL,   -- junction that inserted this spacer
  gap_index    INTEGER NOT NULL,   -- 1-based, in final coordinate order
  start        INTEGER,            -- final coordinates, post trim and rotation
  end          INTEGER,
  gap_bases    INTEGER,
  size_known   INTEGER,            -- 1 = reference estimate, 0 = placeholder
  time_stamp   INTEGER,
  PRIMARY KEY (ID, gap_index)
)
```

`size_known` is the distinction export needs: 1 when the length came from the
reference alignment, 0 when the junction was unmapped and took
`gap_len_default`.

**Positions are stored in final coordinates.** `src_scaffold` is NA at exactly
the bases the join inserted, which gives a per-base spacer map. That map is
reindexed by the circularization trim and the rotation alongside the coverage
vectors, so the recorded intervals describe the sequence as exported. Export
shifts them again by its own linear-end trim, then matches a run of Ns by
overlap.

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

| Run overlaps a spacer | That spacer was sized | `gap_type` | `estimated_length` | `linkage_evidence` |
|---|---|---|---|---|
| yes | yes | `within scaffold` | run length | `align-genus` / `align-xgenus` |
| yes | no | `within scaffold` | `unknown` | `align-genus` / `align-xgenus` |
| no | n/a | `unknown` | run length | none |

`align-genus` when the sample's `gap_evidence.genus_match` is `same`,
`align-xgenus` when `different`. The hyphenated spelling is the feature table's;
the underscore form belongs to AGP, a different file format.

With no stored answer, which is what a headless `export_files()` call gets, no
evidence is claimed. INSDC makes `linkage_evidence` mandatory for `within
scaffold` and invalid otherwise, so a gap we cannot vouch for is emitted as
`gap_type unknown` with the qualifier omitted entirely. `unspecified` is not used:
AGP reserves it for contamination gaps and legacy conversions.

Matching is by POSITION, not by length. Length matching cannot tell a spacer from
a run a scaffold arrived with, and fails outright when a scaffold's terminal Ns
fuse with an adjacent spacer or when its internal assembler gap happens to be the
same size as the placeholder.

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
