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

# blastn/blastdbcmd arrive transitively today (the bioconda getorganelle/mitos/
# trnascan installs pull them in). The WF1 reference search depends on blastn and
# on BLAST DB v5 read support, so pin it rather than rely on a transitive solve.
# Kept with the other base-env conda installs and BEFORE renv::restore: a solve
# here can move libcurl/openssl/zlib, and doing that under R packages already
# compiled against them is how the libidn breakage below happened.
RUN mamba install -y -c bioconda blast=2.16.0

# Local metazoan mitogenome BLAST database for the WF1 reference search.
# 134,560 sequences / 2,201,289,510 bases; 289 MB tarball, 835 MiB unpacked.
# Tagged separately from the package so a database refresh does not need a
# package release, and a package release does not need the asset re-uploaded.
# curl -f is load-bearing: without it a GitHub 404/403 exits 0 and writes an HTML
# body that tar then rejects as "not in gzip format" instead of the real cause.
# Downloaded to a file rather than piped into tar so that curl's exit status is
# actually checked (a pipeline's status is its LAST command's); the file is
# removed in the same RUN, so it does not add to the layer.
# --no-same-owner + chmod a+rX: tar as root would otherwise restore the build
# machine's uid/mode, and Singularity runs as the invoking user, so an unreadable
# database would pass the root-run smoke test here and fail only on HPC.
# blastdbcmd -info is the smoke test (volume-agnostic, unlike a .nsq stat).
# taxonomy4blast.sqlite3 must be present: without it blastn SILENTLY discards
# -taxids and exits 0 with unrestricted hits.
# Placed above COPY renv.lock so a dependency bump does not re-download 289 MB,
# and well above the package COPY so R code edits never invalidate this layer.
ARG MITO_BLASTDB_URL=https://github.com/Smithsonian/MitoPilot/releases/download/blastdb-2026.08/mito_metazoa_blastdb.tar.gz
RUN mkdir -p /ref_dbs && \
    curl -fsSL "$MITO_BLASTDB_URL" -o /tmp/mito_metazoa_blastdb.tar.gz && \
    tar --no-same-owner -xzf /tmp/mito_metazoa_blastdb.tar.gz -C /ref_dbs && \
    rm -f /tmp/mito_metazoa_blastdb.tar.gz && \
    chmod -R a+rX /ref_dbs/mito_metazoa && \
    BLASTDB=/ref_dbs/mito_metazoa blastdbcmd -db /ref_dbs/mito_metazoa/mito_metazoa -info && \
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
