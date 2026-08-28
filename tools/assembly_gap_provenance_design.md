# Assembly gaps in a scaffolded mitogenome

Date: 2026-08-28 (revised)
Status: approved for implementation

## What NCBI told us

Susan Schafer Storz (NCBI/NLM), 2026-08-28, in reply to a direct question about
submitting reference-scaffolded mitogenomes:

1. "We do accept gapped genomes. The number of n's should be the estimated
   length and the correct gene order is expected."
2. "BioSamples can be on more than one sequence so it is not an issue to submit
   multiple fragments."
3. "You really don't need to use assembly_gap features. A regular gap feature is
   fine. Or send with the n's."
4. "If it is an estimated length the feature should be continuous."

This supersedes the earlier design, which was built from the public
documentation alone and reached for `assembly_gap` and its linkage-evidence
vocabulary.

## Consequences

**Every N we insert must be an estimate.** (1) rules out the fixed placeholder
the join used for a junction the reference could not size. A fabricated 100 bp
spacer states a gap length we do not have.

**A sample we cannot honestly join has somewhere to go.** (2) makes leaving it
fragmented a sanctioned outcome rather than a failure.

**The linkage-evidence apparatus is unnecessary.** (3) means a plain `gap`
feature carrying only `/estimated_length`. No `gap_type`, no
`linkage_evidence`, and therefore no need to know whether the ordering
reference shares the sample's genus.

**Genes spanning a gap stay whole.** (4). Splitting a CDS into two partial
features is correct only for a gap of unknown size, and once every gap is an
estimate that case cannot arise from a join.

## Design

### 1. The join refuses a junction it cannot size

`join_scaffolds()` records `size_known` per junction. When any junction in a
sample yields an unsized spacer, the join is **declined**: no Path 0 is built and
the sample stays fragmented, with a note saying which junctions could not be
sized and that its contigs can be submitted as separate sequences.

The same refusal applies to the manual join editor in the app, which reports it
in the dialog rather than silently building a sequence that cannot be submitted.

`gap_len_default` therefore no longer reaches the output. It stays as the
internal marker that produced the unsized junction we refuse on.

### 2. Export writes a plain gap feature

For each run of Ns at or above `gap_min` that overlaps a spacer this pipeline
inserted:

```
<start>	<end>	gap
			estimated_length	<run length>
```

Nothing else. Runs of Ns that the sequence arrived with are **not** declared:
they may be ambiguous base calls rather than gaps, and we have nothing to say
about them. They still contribute the note on any coding feature that contains
them.

### 3. Removed

- The `gap_evidence` table, the export gap-evidence modal, and the genus
  question. They existed only to choose `linkage_evidence`.
- `assembly_gap`, `gap_type`, `linkage_evidence`.
- The CDS split into partial features, per (4).

### 4. Kept

`scaffold_junctions` stays: it is how the join records which bases it inserted
and whether each was sized, which is what the refusal in (1) and the run
classification in (2) both rest on.

## Testing

Unit tests over simulated joins: a sample with only sizable junctions joins and
its gaps export as `gap` features with the estimated length; a sample with an
unsizable junction is declined with a note and no Path 0; a run of Ns the
sequence arrived with is not declared but is still noted on a CDS that contains
it; a gene spanning an estimated gap stays a single continuous feature.
