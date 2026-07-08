# Build the orf314 reference protein BLAST DB from cnidarian GenBank records.
#
# Unlike build_novel_gene_refdbs.R (which queries the NCBI *protein* DB by gene
# name), orf314 is a mitogenome-specific hypothetical ORF with no named RefSeq
# protein family. So instead we download cnidarian mitochondrial *nucleotide*
# records that annotate orf314, extract each orf314 CDS translation from the
# record features, dereplicate, and build the BLAST DB. Not restricted to RefSeq.
#
#   Rscript data-raw/build_orf314_refdb.R
#
# Envs: `mitoreview` (esearch/efetch + Biopython), `blast` (makeblastdb).
# Output: data-raw/novel_gene_refdbs/orf314.fas (+ BLAST index, git-ignored) and
# a provenance TSV. After review, the .fas + index are added into the shipped
# curation tarball ref_dbs/Mitos2/Metazoa_RefSeq235.tar.gz under
# Metazoa_RefSeq235/featureProt/ (see data-raw/novel_gene_refdbs/README.md).
#
# Header contract (per-gene featureProt file): >{accession} {Species}; the gene
# identity comes from the file name (orf314.fas), so headers only need a unique
# first token. We use >{accession}:orf314-1-1-{aalen} {Species}.

EDIRECT <- "mitoreview"   # conda env with esearch/efetch + biopython
BLASTENV <- "blast"       # conda env with makeblastdb
STAGE <- "data-raw/novel_gene_refdbs"
QUERY <- '"Cnidaria"[Organism] AND orf314[All Fields]'
MIN_AA <- 60L             # drop truncated fragments
MAX_PER_GENUS <- 3L       # dereplicate oversampled genera (e.g. Alatina)
dir.create(STAGE, showWarnings = FALSE, recursive = TRUE)

conda_run <- function(env, cmd, intern = TRUE) {
  system(sprintf("conda run -n %s bash -lc %s", env, shQuote(cmd)), intern = intern)
}

gb <- file.path(STAGE, "cnidaria_orf314.gb")
fas <- file.path(STAGE, "orf314.fas")
tsv <- file.path(STAGE, "orf314_provenance.tsv")

# 1. Download all matching cnidarian mito records in full GenBank format.
message("Downloading: ", QUERY)
conda_run(EDIRECT, sprintf(
  'esearch -db nuccore -query %s | efetch -format gb > %s',
  shQuote(QUERY), shQuote(gb)), intern = FALSE)
message("  LOCUS records: ",
        length(grep("^LOCUS", readLines(gb, warn = FALSE), value = TRUE)))

# 2. Extract orf314 CDS translations, dereplicate, write FASTA + provenance.
py <- tempfile(fileext = ".py")
writeLines(sprintf('
import re, sys, csv
from Bio import SeqIO
GB, OUT_FAS, OUT_TSV = sys.argv[1:4]
MIN_AA, MAX_PER_GENUS = %d, %d
PAT = re.compile(r"orf[ _-]?314(?!\\d)", re.IGNORECASE)
def qtext(f):
    v=[]
    for k in ("gene","product","note","standard_name"): v += f.qualifiers.get(k, [])
    return " | ".join(v)
rows=[]; seen={}; fasta=[]
for rec in SeqIO.parse(GB, "genbank"):
    acc = rec.id; org = rec.annotations.get("organism","unknown").strip()
    for f in rec.features:
        if f.type != "CDS" or not PAT.search(qtext(f)): continue
        if "translation" in f.qualifiers:
            aa = f.qualifiers["translation"][0]
        else:
            tt = int(f.qualifiers.get("transl_table",["4"])[0])
            try: aa = str(f.extract(rec.seq).translate(table=tt, cds=False))
            except Exception: continue
        aa = aa.replace("*","")
        if len(aa) < MIN_AA: continue
        rows.append(dict(accession=acc, organism=org, aa_len=len(aa),
                         gene=";".join(f.qualifiers.get("gene",[])),
                         product=";".join(f.qualifiers.get("product",[])), dup_of=""))
        if aa in seen: rows[-1]["dup_of"]=seen[aa]; continue
        seen[aa]=acc; fasta.append((acc, org, aa))
by={}
for acc,org,aa in fasta: by.setdefault(org.split()[0],[]).append((acc,org,aa))
kept=[]
for g,it in by.items():
    it.sort(key=lambda x: len(x[2]), reverse=True); kept += it[:MAX_PER_GENUS]
kept.sort(key=lambda x: (-len(x[2]), x[0]))
with open(OUT_FAS,"w") as fh:
    for acc,org,aa in kept: fh.write(f">{acc}:orf314-1-1-{len(aa)} {org}\\n{aa}\\n")
with open(OUT_TSV,"w",newline="") as fh:
    w=csv.DictWriter(fh, fieldnames=["accession","organism","aa_len","gene","product","dup_of"], delimiter="\\t")
    w.writeheader(); w.writerows(rows)
print(f"orf314 CDS: {len(rows)}; unique: {len(fasta)}; kept after per-genus cap: {len(kept)}")
', MIN_AA, MAX_PER_GENUS), py)
message(conda_run(EDIRECT, sprintf("python %s %s %s %s",
        shQuote(py), shQuote(gb), shQuote(fas), shQuote(tsv))))

# 3. Build the protein BLAST index.
conda_run(BLASTENV, sprintf("makeblastdb -in %s -dbtype prot", shQuote(fas)), intern = FALSE)
message("Staged: ", fas, " (+ BLAST index). REVIEW, then add into the tarball.")
