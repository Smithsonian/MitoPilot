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
    mamba install -c bioconda trnascan-se=2.0.12 -m -n trnascan-se && \
    mamba install -c bioconda mitos=2.1.10 -m -n mitos && \
    mamba install -c bioconda aragorn=1.2.41 -m -n aragorn
#    mamba install -c conda-forge r-base=4.5.2 r-reticulate && \
#    mamba install r-remotes

# Install NCBI ORFfinder (not available on bioconda) into its own conda env.
# orf_finder() invokes it via `conda run -n orffinder ORFfinder`.
RUN mamba create -y -n orffinder -c conda-forge curl && \
    curl -sSL https://ftp.ncbi.nlm.nih.gov/genomes/TOOLS/ORFfinder/linux-i64/ORFfinder.gz \
      | gunzip > /opt/conda/envs/orffinder/bin/ORFfinder && \
    chmod +x /opt/conda/envs/orffinder/bin/ORFfinder

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

# Install MitoPilot — kept last so R code edits only invalidate this layer
COPY docker/MitoPilot_*.tar.gz /pkg.tar.gz
RUN R -e 'remotes::install_local("/pkg.tar.gz",upgrade="never")'
RUN rm /pkg.tar.gz

# Default Reference DBs
# not needed for >= v1.3.0, since dbs are downloaded directly from GitHub
#COPY ref_dbs/getOrganelle /ref_dbs/getOrganelle
#COPY ref_dbs/Mitos2 /ref_dbs/Mitos2
