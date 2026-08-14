FROM condaforge/mambaforge:24.9.2-0

# Passed from Github Actions
ARG GIT_VERSION_TAG=unspecified

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y \
        jq \
        parallel \
        default-jre \
        python2.7 \
        build-essential \
        automake \
        autoconf && \
    rm -rf /var/lib/apt/lists/*

RUN conda config --set channel_priority true && \ 
    conda config --add channels defaults && \
    conda config --add channels conda-forge && \
    conda config --add channels bioconda 

RUN mamba install -y -c conda-forge libtiff libxml2 libxml2-devel r-base=4.5.2 r-reticulate r-remotes r-ragg

# Install Conda packages
RUN mamba install -c bioconda fastp=0.23.4 && \
    mamba install -c bioconda spades=4.1.0 && \
    mamba install -c bioconda getorganelle=1.7.7.1 && \
    mamba install -c bioconda bam-readcount=1.0.1 -m -n bam-readcount && \
    mamba install -c bioconda bowtie2=2.5.4 && \
    mamba install -c bioconda samtools=1.21 && \
    mamba install -c bioconda minimap2=2.28 && \
    mamba install -c bioconda trnascan-se=2.0.12 -m -n trnascan-se && \
    mamba install -c bioconda mitos=2.1.10 -m -n mitos && \
    mamba install -c bioconda aragorn=1.2.41 -m -n aragorn
#    mamba install -c conda-forge r-base=4.5.2 r-reticulate && \
#    mamba install r-remotes

# blastn/blastdbcmd/makeblastdb arrive transitively (the bioconda getorganelle/
# mitos/trnascan installs above pull them in), already at 2.16.0+ in
# /opt/conda/bin. The WF1 reference search hard-depends on blastn and on BLAST
# DB v5 read support, so assert rather than reinstall: an explicit
# `mamba install blast=2.16.0` here cost a 1.08 GB layer for binaries the image
# already had. This check is free and still fails the build loudly if a future
# dependency solve drops or downgrades BLAST, which is exactly when to find out.
# Accepts any version >= 2.16, including a future 3.x.
RUN for t in blastn blastdbcmd makeblastdb; do \
        command -v "$t" >/dev/null || \
            { echo "BLAST+ tool '$t' is missing: a dependency bump dropped it"; exit 1; }; \
    done && \
    set -- $(blastn -version | head -1 | sed -E 's/^blastn: ([0-9]+)\.([0-9]+)\..*/\1 \2/') && \
    { case "$1$2" in ''|*[!0-9]*) \
        echo "could not parse blastn version: $(blastn -version | head -1)"; exit 1;; esac; } && \
    { [ "$1" -gt 2 ] || { [ "$1" -eq 2 ] && [ "$2" -ge 16 ]; } || \
        { echo "BLAST+ $1.$2 is older than the required 2.16"; exit 1; }; } && \
    echo "BLAST+ $1.$2 present"

# Local metazoan mitogenome BLAST database for the WF1 reference search.
# 134,560 sequences / 2,201,289,510 bases; 289 MB tarball, 835 MiB unpacked.
#
# Staged into the build context by the maintainer, exactly like
# docker/MitoPilot_*.tar.gz. Build it with tools/build_local_blast_db.py +
# tools/dedup_local_blast_db.py, then copy the tarball to
# docker/mito_metazoa_blastdb.tar.gz. It is gitignored: at 275 MiB it exceeds
# GitHub's 100 MB per-file push limit, and it ships to users inside this image
# rather than through the repo. The deploy scripts check for it up front.
#
# ADD (not COPY + tar) extracts a local tarball into a SINGLE layer and leaves no
# tarball behind; COPY followed by a separate RUN would keep the 275 MiB archive
# in its own layer forever. --chown is required because ADD otherwise preserves
# the build machine's uid/gid from the archive. Do not add a `chmod -R` in a
# later RUN: touching every file's metadata copies the whole 835 MiB layer again.
# The archive is already 0644/0755, so it is readable by the invoking user under
# Singularity.
#
# blastdbcmd -info is the smoke test (volume-agnostic, unlike a .nsq stat).
# taxonomy4blast.sqlite3 must be present: without it blastn SILENTLY discards
# -taxids and exits 0 with unrestricted hits.
# Placed above COPY renv.lock so a dependency bump does not rebuild this layer,
# and well above the package COPY so R code edits never invalidate it.
ADD --chown=root:root docker/mito_metazoa_blastdb.tar.gz /ref_dbs/
RUN BLASTDB=/ref_dbs/mito_metazoa blastdbcmd -db /ref_dbs/mito_metazoa/mito_metazoa -info && \
    test -s /ref_dbs/mito_metazoa/taxonomy4blast.sqlite3

RUN R -e "remotes::install_github('rstudio/renv')"
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

# install MitoFinder
RUN git clone https://github.com/RemiAllio/MitoFinder.git /opt/MitoFinder
RUN cd /opt/MitoFinder;./install.sh;cd ~
RUN chmod -R 777 /opt/MitoFinder
ENV PATH="/opt/MitoFinder:${PATH}"

# Install ARWEN (mitochondrial tRNA prediction)
COPY docker/arwen/arwen1.2.3.c /opt/arwen/arwen.c
RUN gcc -O2 -ffast-math -o /opt/arwen/arwen /opt/arwen/arwen.c -lm
ENV PATH="/opt/arwen:${PATH}"

RUN ln -s /usr/lib/x86_64-linux-gnu/libidn2.so.0 /usr/lib/x86_64-linux-gnu/libidn.so.11

# Install NCBI ORFfinder (not available on bioconda) into its own conda env.
# orf_finder() invokes it via `conda run -n orffinder ORFfinder`.
# libuv (libuv.so.1) and elfutils (libdw.so.1) are runtime deps of the NCBI
# ORFfinder binary. `conda run` does not add $CONDA_PREFIX/lib to the loader
# path, so patchelf sets an RPATH ($ORIGIN/../lib) so the binary finds them.
# Placed after the slow renv/MitoFinder layers so edits here rebuild cheaply.
RUN mamba create -y -n orffinder -c conda-forge curl libuv elfutils patchelf && \
    curl -sSL https://ftp.ncbi.nlm.nih.gov/genomes/TOOLS/ORFfinder/linux-i64/ORFfinder.gz \
      | gunzip > /opt/conda/envs/orffinder/bin/ORFfinder && \
    chmod +x /opt/conda/envs/orffinder/bin/ORFfinder && \
    /opt/conda/envs/orffinder/bin/patchelf --set-rpath '$ORIGIN/../lib' /opt/conda/envs/orffinder/bin/ORFfinder

# Install MitoPilot — kept last so R code edits only invalidate this layer
COPY docker/MitoPilot_*.tar.gz /pkg.tar.gz
RUN R -e 'remotes::install_local("/pkg.tar.gz",upgrade="never")'
RUN rm /pkg.tar.gz

# Default Reference DBs
# not needed for >= v1.3.0, since dbs are downloaded directly from GitHub
#COPY ref_dbs/getOrganelle /ref_dbs/getOrganelle
#COPY ref_dbs/Mitos2 /ref_dbs/Mitos2
