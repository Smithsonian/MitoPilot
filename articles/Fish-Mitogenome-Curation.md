# Fish Mitogenome Curation

Automated curation and validation of mitogenome annotations is a central
goal of MitoPilot. When samples are processed through the Annotation
Module, annotations are first generated using a combination of
[MITOS2](https://gitlab.com/Bernt/MITOS) (for rRNA and PCG annotation)
and [tRNAscan-SE](https://github.com/UCSC-LoweLab/tRNAscan-SE) (for tRNA
annotation). Following the initial annotation, samples are processed
through an automated curation process where modifications are made in
attempt to bring the annotations closer to a state that will be
considered acceptable for publication by NCBI GenBank. This automated
curation process is based on two important inputs to the pipeline, a
high-quality reference database of protein coding genes and a set of
user configurable parameters.

## Reference Databases

MitoPilot expects the curation reference database files to be in the
form of Blastp database files, one for each gene. By default, the
pipeline will use the same database files used by Mitos2 during the
initial annotation process. This can be modified using the `ref_db`
property of the Curation Parameters (see below). The default value of
ref_db,
(`ref_db = list(default = "/ref_dbs/Mitos2/Chordata/featureProt/{gene}.fas")`),
specifies the location on the MitoPilot docker image where the database
is located, where `{gene}` is translated to individual gene names during
processing. This default database location can be overridden by
gene-specific paths added as named entries to the ref_db parameter,
e.g. `ref_db = list(default = "/ref_dbs/Mitos2/Chordata/featureProt/{gene}.fas", atp8 = "/ref_dbs/curation/atp8.fas")`.
However, it is important to note that these paths are relative to the
execution environment and must be accessible from that environment.
Building a custom docker image from the base MitoPilot image, including
any additional reference databases within its file system, is one way to
achieve this.

## Curation Parameters

Currently, curation parameters are specified at project initialization
and are applied to all samples within the project. By default, the
parameters are set by the function,
[`params_fish_mito()`](https://smithsonian.github.io/MitoPilot/reference/params_fish_mito.md).
Customization of the parameters can be achieved by directly providing a
complete named list, or by passing individual modifications to the
[`params_fish_mito()`](https://smithsonian.github.io/MitoPilot/reference/params_fish_mito.md)
function. For example, the default expected count of trnW genes could be
increased to 2 and the default percent similarity of an “acceptable” PCG
match could be reduced to 85 by initializing a new project with:

``` r

new_project(
  ...,
  curate_params = params_fish_mito(
    list(
      hit_threshold = 85,
      rules = list(
        trnW = list(
          count = 2
        )
      )
    )
  )
)
```

The full set of default curation parameters can be viewed in the
MitoPilot GUI for each sample under “Curation Opts”. At the moments,
these values can not be edited for individual samples from within the
GUI, but this feature will eventually be added. Developer Note: the
curation parameters are stored in the project sqlite database as a
base64 encoded text that must be parsed for reading / writing.

## Validation

Following the automated curation process, samples are processed through
a validation script,
[`validate_fish_mito()`](https://smithsonian.github.io/MitoPilot/reference/validate_fish_mito.md)
by default. This script looks for anomalies in the final annotations
relative to the expectations specified in the curation parameters and
add notations to assist with manual curation using the MitoPilot GUI.

- **possible duplicate**: The annotation occurs in the assembly more
  often than expected. All identical annotations will receive this flag
  and the user must determin which (if any) should be removed.
- **exceeds max overlap**: The annotation overlaps one or more
  neighboring annotations on the same strand by a percentage of its
  length greater than the value set by the max_operlap parameter of the
  curation params. This represents a global maximum overlap threshold.
- **exceeds max start overlap**: The start position of a PCG annotation
  overlaps one or more neighboring annotations on the same strand more
  than expected.
- **exceeds max stop overlap**: The stop position of a PCG annotation
  overlaps one or more neighboring annotations on the same strand more
  than expected.
- **exceeds max length**: The annotation length exceed the maximum
  expectation set in the curation params.
- **below min length**: The annotation length is below the minimum
  expectation set in the curation params.
- **low coverage region**: More than 5% of the assembly region defined
  by an annotation has less then 10x coverage.
- **high error region**: More than 5% of the assembly region defined by
  an annotation has a raw read error rate greater than 5% (ie, more than
  5% of raw reads than align to the base show disagreement on the base
  call).
- **internal stop codon**: An internal stop codon was detected in a PCG
  annotation.
- **non-standard stop codon**: A PCG stop codon other than those
  specified in the curation params was detected.
- **non-standard start codon**: A PCG start codon other than those
  specified in the curation params was detected.
- **low reference similarity**: The PCG curation reference database does
  not include any references with compositional similarity equal to ro
  greater than the hit_threshold set in the curation parameters (default
  = 90).
- **check reference start alignment**: The PCG start position does not
  align exactly with the majority of the top hits from the reference
  database.
- **check reference stop alignment**: The PCG stop position does not
  align exactly with the majority of the top hits from the reference
  database.
