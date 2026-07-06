# Splice a joined gene's segments into one CDS

Concatenates a join group's segment (exon) sequences in 5'-\>3' order
and translates them, matching the export-time splice logic so the manual
editor and the exported feature table agree. Used both by \[export()\]
and by the annotate-details editor's spliced-CDS preview / alignment.

## Usage

``` r
splice_join_cds(members, seq, genetic_code)
```

## Arguments

- members:

  data frame of the group's annotation rows. Requires columns pos1,
  pos2, direction, start_codon, stop_codon, partial_start, partial_stop.

- seq:

  the scaffold assembly sequence (DNAString/DNAStringSet) the members
  belong to.

- genetic_code:

  a resolved Biostrings genetic-code vector (e.g.
  \`session\$userData\$gcode\` or \`Biostrings::getGeneticCode("2")\`).

## Value

list with \`dna\`, \`translation\` (terminal stop stripped),
\`start_codon\`, \`stop_codon\`, \`partial_start\`, \`partial_stop\`,
\`pos1\`, \`pos2\`, \`length\`, \`direction\`, and \`segments\`: a data
frame (one row per exon, ordered 5'-\>3') with \`member_row\` (index
into \`members\`), \`pos1\`, \`pos2\`, \`aa_start\`, \`aa_end\`.
