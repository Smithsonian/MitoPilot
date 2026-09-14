# MapToRef: bwa-mem as a second mapper, relaxed first pass

Date: 2026-09-14
Branch: feat/maptoref-mapper-choice, off map-to-ref-assembly
Status: specified

## Goal

MapToRef maps reads with bowtie2 only (`R/map_to_ref.R:593-700`). Two changes:

1. Let a parameter set choose `bwa-mem` instead of `bowtie2`. bwa-mem recovers
   more reads than bowtie2 when the reference is 10-20% divergent, which is
   the case MapToRef exists for.
2. Make the first mapping pass more permissive than the later ones, the way
   Geneious's high-sensitivity mapper does. Pass 1 maps against the user's
   distant reference and is where divergence bites. Passes 2 onward map against
   a consensus built from the sample's own reads, so the default stringency is
   right there.

Out of scope: BBMap, minimap2 for short reads, multi-bait consensus, native
circular mapping. Geneious's per-read realignment step has no open equivalent
and is not attempted.

## What Geneious does that we can mimic

From the Geneious manual (AssemblyMapping): seed-and-extend with 1-mismatch seed
search at higher sensitivity, seeds spanning the origin for circular
references, and iterations that let reads extend past reference ends.

| Geneious behaviour | MapToRef today | This change |
|---|---|---|
| 1-mismatch seeds on divergent refs | `--very-sensitive-local` only | relaxed seeding on pass 1 |
| circular origin wrap | 500 bp flank pad (`.mtr_extend`) | unchanged |
| reads extend past ends each iteration | flank re-padded from consensus each pass | unchanged |
| per-read realignment around indels | none | not attempted |

## Decisions

1. **One options column serves both mappers.** `assemble_opts.maptoref` keeps
   holding the flag string; its meaning depends on the new `maptoref_mapper`
   column. No second flag column.
2. **Mapper is a parameter-set choice**, stored in `assemble_opts`, not
   per-sample. Per-sample already exists for the reference; the mapper is
   pipeline policy.
3. **Relaxed pass 1 is always on**, not a toggle. It is logged, and the
   relaxed flags are appended after the user's flags so both tools let them
   win. A toggle can follow if real data asks for it.
4. **Final pass uses the user's flags**, same as passes 2+. The final BAM feeds
   the pileup viewer and depth track, so it should reflect the configured
   stringency, not the permissive first pass.
5. **bwa-mem output is filtered to match bowtie2's.** bwa emits unmapped
   records with no `--no-unal` equivalent; add `samtools view -F 4` where
   bowtie2 used `--no-unal`. Pass 1 keeps unmapped mates (`-G 12` filter,
   already there) for both mappers.

## Mapper commands

`cpus`, `idx`, `r1`, `r2` as today. `{opts}` is the effective flag string for
the pass (see next section).

| | bowtie2 | bwa-mem |
|---|---|---|
| index | `bowtie2-build -q ref idx` | `bwa index -p idx ref` |
| map | `bowtie2 {opts} -x idx -1 r1 -2 r2 --threads cpus` | `bwa mem -t cpus {opts} idx r1 r2` |
| drop unaligned (passes 2+, final) | `--no-unal` | pipe through `samtools view -b -F 4` |
| stderr | `2>> log` | `2>> log` |

Output handling after the mapper (`samtools view -G 12`, `sort`, `index`,
`consensus`, `depth`, `.mtr_count_primary` with `-F 0x904`) is unchanged and
tool-agnostic. bwa hard-clips supplementaries; `samtools consensus` ignores
clipped bases, so consensus is unaffected.

## Effective flags per pass

| pass | bowtie2 | bwa-mem |
|---|---|---|
| 1 (user reference) | `{user} -N 1 -L 15 -i S,1,0.25 --mp 4,2 --score-min G,10,6` | `{user} -k 15 -B 2 -T 20` |
| 2..n (own consensus) | `{user}` | `{user}` |
| final | `{user}` | `{user}` |

Defaults for `{user}`:

- bowtie2: `--very-sensitive-local` (unchanged, `.mtr_default_bowtie2`)
- bwa-mem: empty string; bwa mem defaults are already local with soft
  clipping. Add `.mtr_default_bwa <- ""`.

Rationale for the relaxed sets: bowtie2 `-N 1` allows a seed mismatch, `-L 15`
shortens seeds, `-i S,1,0.25` seeds more densely, `--mp 4,2` and
`--score-min G,10,6` accept more mismatches per read. bwa `-k 15` shortens the
minimum seed, `-B 2` halves the mismatch penalty, `-T 20` lowers the output
score floor. Both tools take the last occurrence of a repeated flag, so
appending after `{user}` makes the relaxed values win on pass 1.

The relaxed pass admits more off-target reads (NUMTs, contaminants). They only
influence the pass 1 consensus, which is then re-mapped at default stringency.
The existing `reads_pass_1 < 100` failure and `< 1000` warning stay on the
pass 1 count.

## Code changes

### `R/map_to_ref.R`

- `map_to_ref()` gains `mapper = "bowtie2"` after `bowtie2_opts`. Keep the
  `bowtie2_opts` argument name for backwards compatibility of direct callers;
  document it as "flags for the chosen mapper". Validate `mapper %in%
  c("bowtie2", "bwa-mem")` and fail via `.mtr_fail` otherwise.
- New helpers, pure string builders so they unit-test without tools:
  - `.mtr_index_cmd(mapper, ref_fa, idx)`
  - `.mtr_map_cmd(mapper, opts, idx, r1, r2, cpus, log_fn, drop_unal)`
    returns the mapper command up to and including the stderr redirect and,
    when `drop_unal` and mapper is bwa-mem, `| samtools view -b -F 4 -`.
  - `.mtr_pass_opts(mapper, user_opts, pass)` returns the effective flag
    string; `pass = 1L` appends the relaxed set, anything else returns
    `user_opts`.
  - `.mtr_check_tools(mapper)` stops with a clear message when the mapper
    binary (and `bowtie2-build` or `bwa`) is missing from PATH.
- `.mtr_assemble()` replaces the four inline bowtie2 blocks (pass 1, loop,
  final) with the helpers. Log the effective flags once per pass:
  `pass 1 (bowtie2): --very-sensitive-local -N 1 ...`.
- `iterations.tsv` gains no column; the log carries the flags.

### Schema and options

- `R/init_db.R`: `maptoref_mapper = "bowtie2"` parameter on `new_db()` and in
  the `assemble_opts` default row; `.mtr_default_bwa`.
- `R/backwards_compatibility.R`: add `maptoref_mapper` column (default
  `"bowtie2"`) alongside the existing maptoref column migration near line
  1324; extend the column check near line 182.
- `R/app_assemble_utils.R:400-415`: `selectInput(ns("maptoref_mapper"),
  choices = c("bowtie2", "bwa-mem"))` above the options text box. Relabel the
  text box "MapToRef mapper options". Help text lists presets per mapper and
  states that pass 1 is run with relaxed seeding. When the mapper select
  changes and the options box holds the other mapper's default, swap it to the
  new mapper's default (observer in `R/app_assemble.R` next to the existing
  `maptoref` handlers at 838, 844, 885, 940, 1025).
- `R/app_assemble.R:1025` area: persist `maptoref_mapper`.

### Nextflow

- `inst/nextflow/modules/assemble_workflow.nf:25,129`: select and map
  `opts.maptoref_mapper` (default `'bowtie2'` when null).
- `inst/nextflow/modules/assemble.nf:106`: pass `mapper = '!{opts.maptoref_mapper}'`
  as a named argument to `MitoPilot::map_to_ref(...)`.
- `assemble.nf:107`: prefix `opts.txt` with the mapper name.

### Container

- `docker/Dockerfile:31`: add `mamba install -c bioconda bwa=0.7.19`.
- `docker/README.md`: list bwa among pinned tools.
- Bump image tag per existing release process; note in `NEWS.md`.

### Coverage module

`inst/nextflow/modules/coverage.nf:40` and `R/coverage.R:66` use bowtie2 for
the post-assembly coverage step for every assembler. Leave as is; that step
maps to the finished assembly, not a divergent reference.

## Tests

`tests/testthat/test-map-to-ref-loop.R` and a new `test-map-to-ref-mapper.R`:

1. `.mtr_pass_opts()`: pass 1 appends the relaxed set for each mapper; pass 2
   and `"final"` return user flags untouched; empty user flags handled.
2. `.mtr_map_cmd()`: bwa-mem with `drop_unal = TRUE` contains
   `samtools view -b -F 4`; with `FALSE` it does not; bowtie2 with
   `drop_unal = TRUE` contains `--no-unal`. Thread count placed correctly.
3. `map_to_ref(mapper = "hisat")` writes the failure sentinel with the
   validation message.
4. Existing loop tests run unchanged with the default mapper.
5. Loop tests parameterised over `mapper`, with `skip_if(!nzchar(Sys.which("bwa")))`
   for bwa-mem: converges, produces `final.bam`, `maptoref_depth.csv`, same
   consensus length as bowtie2 on the synthetic fixture.
6. `assembler.log.txt` contains `pass 1 (bwa-mem):` with `-k 15`.
7. Schema: `new_db()` writes `maptoref_mapper = "bowtie2"`; migration adds the
   column to an old db (`tests/testthat/test-map-to-ref.R:380` neighbourhood).
8. Nextflow contract test, if one exists for the assemble module, checks the
   new argument is threaded.

## Migration

Existing projects get `maptoref_mapper = "bowtie2"` and keep their flag
string. Behaviour change for them: pass 1 becomes relaxed. Call this out in
`NEWS.md` under the MapToRef section, with the exact flags.

## Open questions

None blocking. If pass 1 relaxation proves to over-recruit on real data, the
fallback is a boolean `maptoref_relax_first` column defaulting TRUE.
