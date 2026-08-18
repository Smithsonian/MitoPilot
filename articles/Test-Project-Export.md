# Test Project: Export

Test project: [1.
Assemble](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Assemble.md)
[2.
Annotate](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Annotate.md)
3. Export

The Export module turns finished annotations into the files GenBank
needs for submission: a FASTA of the sequences and a five-column feature
table of the annotations. MitoPilot allows you to export entire
mitogenomes, individual genes, or both. Export also generates GFF3
annotation files and alignment summaries.

![Export module sample table](figures/get-started/export-table.png)

Export module sample table

## Make a group

Export works on **groups**, which correspond to submission batches.
Select the samples you want and click **ASSIGN GROUP**.

![Submission group window](figures/get-started/export-group-modal.png)

Submission group window

The window summarizes what you selected: how many records, which
topologies, and the gene order of each. Type a name (letters, numbers,
dashes, and underscores only) and click **Create**.

For the test project, select everything. MitoPilot then stops you:

![Warning about mixing complete and partial mitogenomes in one
group](figures/get-started/export-mixed-warning.png)

Warning about mixing complete and partial mitogenomes in one group

A GenBank submission may contain complete mitogenomes or partial
mitogenomes, but not both. This selection has some of each. **Split into
two groups** produces `fish_batch-complete` and `fish_batch-partial`.
Note that completeness is derived from each assembly’s topology, with
circular meaning complete and linear meaning partial, unless you
overrode it using the per-sample Partial flag or the `linear_complete`
project setting.

![Export table with samples assigned to
groups](figures/get-started/export-table-grouped.png)

Export table with samples assigned to groups

A unit can only belong to one export group at a time. **CLEAR GROUP**
removes the selected samples from their group. Re-assigning a grouped
sample asks for confirmation first, because files already written for
the old group are not cleaned up.

## Export

Click **EXPORT DATA**.

![Export Data window with header template and output
options](figures/get-started/export-modal.png)

Export Data window with header template and output options

**Header Template** is the FASTA definition line. Any extra columns you
put in the mapping file at project setup are available here and can be
referenced using `{column_name}`. This is how you can include things
like voucher numbers and BioSample accessions in the GenBank submission
files.

There are two special parameters. First, `{completeness}` expands to
“complete genome” or “partial genome” and belongs at the end of the
header for GenBank to parse it correctly. Second, `{seqid}` takes the
value in the ID column of the mapping file and appends the scaffold/path
number if that sample is exporting multiple sequences.

Templates are validated as you type and can be saved by name for reuse.

Other options in the Export Data window:

- **Generate Group-level PCG alignment summary** aligns every
  protein-coding gene across the group and writes an HTML report. Worth
  the wait as a final check.
- **Export individual protein-coding and rRNA genes** writes per-gene
  FASTA files and feature tables, with their own header template. Useful
  for phylogenetics or single-gene submissions.
- **Review PCG annotations for outliers** runs a final pre-submission
  check described below.

Then click **Export**.

**Note.** If any sample in the group contributes more than one record,
MitoPilot asks you to confirm before exporting. Each scaffold becomes
its own GenBank record with a `ID_p<path>_s<scaffold>` SeqID. This
allows you to submit samples that have truly fragmented mitogenomes.
Samples that still have more than one assembly path cannot be exported
at all, since different assembly paths usually represent multiple
versions of the same molecule.

## The outlier review

With the review enabled, MitoPilot aligns each protein-coding gene
across the group and stops on any gene where a sample looks out of line.

![PCG annotation outlier review with a flagged
sample](figures/get-started/export-outlier-review.png)

PCG annotation outlier review with a flagged sample

A sample’s gene is flagged when it appears truncated or extended
compared to the rest of the submission group, as determined by the
`offset` parameter settings.

Sequences are also flagged when their identity compared the rest of the
group falls below the specified threshold. In the screenshot above,
SRR21844202’s ATP6 is at 59.2% identity against the others.

You can page through the flagged genes with **Prev** and **Next**, jump
straight into the annotation editor for any sample with **edit**, mark a
gene resolved once you have looked at it, or cancel the export entirely.
You can also edit genes for samples that were not automatically flagged.

Files are written only when you click **Done**, so edits made during
review will appear in the exported files.

**Tip.** Low identity is not automatically an error. ATP8 and NAD6 are
genuinely fast-evolving and routinely flag in a diverse export group.

## Exported files

During export, files are saved to `<project>/out/export/<group name>/`:

| File | What it is |
|----|----|
| `<group>.fasta` | The mitogenome sequences with your header template applied |
| `<group>.tbl` | The [five-column feature table](https://www.ncbi.nlm.nih.gov/genbank/feature_table/) of annotations |
| `<group>_sample_info.csv` | The sample metadata behind the submission |
| `AA_alignments_<group>.html` | The group-level protein alignment report |
| `GFFs/` | One [GFF3](https://gmod.org/wiki/GFF3) per sample, can be reviewed using Geneious or other tools |
| `genes/` | Per-gene FASTA and feature tables, if you asked for them |

The FASTA and the feature table are the two files you need for a
[GenBank organelle
submission](https://www.ncbi.nlm.nih.gov/genbank/organelle_submit/).

The HTML report is the file to read first.

![Exported HTML report listing flagged outliers and gene
alignments](figures/get-started/export-aa-report.png)

Exported HTML report listing flagged outliers and gene alignments

It opens with every flagged outlier in one table, then shows the
alignment for each protein-coding gene across the group. Poor
annotations tend to be obvious here.

HYDRA **Getting files off the cluster.** Export runs inside the app
rather than as a cluster job. You will need to [download the
data](https://confluence.si.edu/spaces/HPC/pages/163152227/Transferring+Files+to+from+Hydra)
to submit to GenBank.

Congratulations, you’ve completed the test project walkthrough! When you
are ready to try MitoPilot with your own data, see [starting your own
project](https://smithsonian.github.io/MitoPilot/articles/Your-Own-Project.md).

[Next: Start your own project
→](https://smithsonian.github.io/MitoPilot/articles/Your-Own-Project.md)
