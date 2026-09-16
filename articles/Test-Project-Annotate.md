# Test Project: Annotate

Test project: [1.
Assemble](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Assemble.md)
2. Annotate [3.
Export](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Export.md)

This module finds the genes, curates the gene models against reference
sequences from GenBank, and validates the result against rules for your
taxonomic group. It attempts to flag any issues that would cause a
rejection during GenBank submission.

Each row here is one **assembly**, not one sample. A sample that kept
several scaffolds or paths will be represented by several rows, and each
is annotated and validated on its own.

## Set the options

**Annotate Opts.** controls the annotation tools.

![Annotation options window](figures/get-started/annotate-opts.png)

Annotation options window

Genes come from [MITOS2](https://gitlab.com/Bernt/MITOS) (protein-coding
genes, tRNAs, and rRNAs) and
[tRNAscan-SE](https://github.com/UCSC-LoweLab/tRNAscan-SE) (tRNAs), with
MitoFinder, ARWEN, ARAGORN, and ORFfinder available as optional
annotators. The MITOS2 reference database defaults to `Chordata`, but
`Metazoa_RefSeq89` is the general-purpose choice for other groups.

**Curate Opts.** controls what MitoPilot does with those raw
annotations.

![Curation options window showing the taxonomic
ruleset](figures/get-started/curate-opts.png)

Curation options window showing the taxonomic ruleset

The setting that matters most is **Target**, the taxonomic ruleset. It
sets the expected gene content, the allowed start and stop codons, the
naming conventions, and the genetic code. It defaults to Actinopterygii,
which is correct for the test fishes.

For your own data, pick the closest clade from the [ruleset
browser](https://smithsonian.github.io/MitoPilot/articles/Ruleset-Browser.md).
Expand **Curation rules for this ruleset** to see the validation
parameters, which are the ruleset itself, laid out so you can see
exactly which rules an assembly is being judged against.

**No ruleset for your samples?** If you do not see an appropriate clade
for your samples, please post an
[issue](https://github.com/Smithsonian/MitoPilot/issues) or reach out to
Dan MacGuigan directly at <macguigand@si.edu>. We are always looking to
expand the taxonomic scope of MitoPilot.

When ready, click **Update** and run the workflow the same way you ran
Assemble.

This module is slower than Assemble. MITOS2 takes a few minutes per
assembly, and curation aligns every protein-coding gene against
reference sequences. When working on a computing cluster with your own
project, consider running this step as a job rather than directly from
the MitoPilot app.

## Read the results

![Annotate table showing missing genes, extra genes, and warning
counts](figures/get-started/annotate-table-warnings.png)

Annotate table showing missing genes, extra genes, and warning counts

Scroll right in the table for the columns that matter:

- **\# PCGs, \# tRNAs, \# rRNAs**: how many of each gene type were
  annotated. For a vertebrate mitogenome you expect 13, 22, and 2.
- **Missing**: expected genes that were not found.
- **Extra**: genes annotated more times than the ruleset expects.
- **Warnings**: how many validation flags were raised. This is your work
  queue.

A few assemblies stand out in the test project. For example, SRR21844202
has an extra `trnW` and two warnings.

SCAFFJOIN is missing `atp8`, `trnK`, and `trnL` and carries ten
warnings. This is a real consequence of joining scaffolds across
coverage gaps. The sequence in those gaps is set to “N”, so the genes in
those gaps cannot be annotated.

**Note.** The **Warnings column includes** dropdown menu at the top
filters which warning types are counted. Narrow it to one warning type
to pull out every assembly with that problem and work through them as a
batch.

## Inspect a sample

Click **Details** on any row. Here we’ll look at sample SRR19434536.

![Annotation details window with the gene
table](figures/get-started/annotate-details.png)

Annotation details window with the gene table

The annotation table lists every gene with its position, strand, and the
tool that called it. The **Notes** column records changes made during
automatic curation, such as a start position moved upstream or a stop
codon trimmed. **Warnings** shows what validation flagged. The `nt` and
`aa` buttons copy the nucleotide or amino acid sequence to your
clipboard.

The badges along the top track the assembly: topology, species ID
verified, reviewed, problematic, and partial. The **Mark …** buttons
beside them toggle each flag, which is how you keep track of what you
have already looked at across a large project.

Below the table are four collapsible sections.

**Sequence** shows read depth and per-base error rate along the
assembly, the gene models drawn in lanes below them, then the
nucleotides and translated amino acids once you zoom in. Each track can
be hidden with its checkbox. Error rate bars turn orange above 5%, a
sign of possible sequencing or assembly errors.

![Coverage map with gene models drawn over read
depth](figures/get-started/annotate-coverage-map.png)

Coverage map with gene models drawn over read depth

**BLAST Reference Synteny** lines your annotation up against the closest
GenBank mitogenome, with a percent-identity bar between them. Click
anywhere in this plot to show a zoomed-in base-pair level alignment of
your sample versus the reference.

The reference mitogenome shown in this plot is recorded in the export’s
`sample_info.csv` in the `ref_comparison` column, as “compared sample
XXX to GenBank accession XXX”. It is not written to the FASTA headers.
If the reference mitogenome is a poor match, you can flag it (the CSV
column is then left blank) or use the dropdown menu to pick a better
reference from among the top BLAST hits.

![Gene order compared against the closest GenBank
reference](figures/get-started/annotate-synteny.png)

Gene order compared against the closest GenBank reference

**Alignment** shows a selected protein-coding gene aligned to its
reference hits and the curation database.

![Protein alignment of a gene against its reference
hits](figures/get-started/annotate-alignment.png)

Protein alignment of a gene against its reference hits

The nucleotide boundary box shows the exact sequence at each end of the
gene with the codon frame marked, along with the start and stop codons
that were called. Below it, the protein alignment shows your gene
(“focal”) alongside the reference proteins.

Below the annotation table, the **Sequence** section shows the assembly
itself: genes as arrows in lanes, the nucleotides once you zoom in, and
the translated amino acids of each protein-coding gene under their
codons. Click a row in the table to jump to that gene, or use Whole
genome to step back out. The view follows every edit you make in this
window.

## Manually fix annotations

Click **Edit** in the alignment section to nudge the start or stop
position and watch the alignment respond.

![Alignment-based annotation
editing](figures/get-started/annotation-codon-edits.png)

Alignment-based annotation editing

By default, the `+` and `-` buttons search for the next valid start or
stop codon (according to the curation ruleset you selected). You can
tick the **single codon** checkbox to instead nudge the position one
codon at a time. This can lead to a **partial** gene model with
undetermined start or stop codons.

Clicking the `poly-A stop` button will truncate a stop codon to **TA**
or **T**. Sometimes this is required by GenBank to avoid overlapping
gene models. When transcribed, these genes will have their stop codon
completed by the addition of the 3’ poly-A tail.

You can manually set a gene to have an undetermined start or stop codon
by clicking the Partial `5'` or `3'` buttons. MitoPilot will add the
appropriate format and notes for that gene in the GenBank submission
files. Best to use this sparingly, as GenBank may not accept a
submission with many “partial” gene models.

Other useful editing tools:

- **Merge PCGs/rRNAs** joins gene models that were split into separate
  pieces, which is how you handle spliced or fragmented genes.
- **Auto-assign ORFs** renames open reading frames found by ORFfinder
  based on sequence similarity with the curation database of
  protein-coding genes.
- **Delete** removes an annotation entirely. Bring back a deleted
  annotation using the **Restore** button.
- **Trim unannotated ends** cuts assembly overhang that carries no
  genes, can be undone.
- **Linearize** converts a circular assembly to linear, useful when the
  control region assembled poorly.
- **Align fewer refs** in the Alignment panel speeds up editing by
  restricting the alignment to the top five hits, which matters because
  the alignment is recomputed on every start/stop codon nudge.

You can record what you did in the **Notes** section; it saves
automatically and is retained with the assembly.

**Warning.** Validation warnings do not disappear when you fix the
underlying problem. They record the state at the time the Annotate
module ran. Use the **Mark Reviewed** button to track what you have
actually dealt with.

## Lock and move on

When you are satisfied, select the assemblies and click **Lock** to
release them to the Export module.

[Next: Export
→](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Export.md)
