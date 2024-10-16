FROM condaforge/mambaforge

RUN apt-get update && \
    apt-get install -y jq && \
    apt-get install -y parallel && \
    rm -rf /var/lib/apt/lists/*

# Install Conda packages
RUN conda config --add channels defaults && \
    conda config --add channels conda-forge && \
    conda config --add channels bioconda && \
    mamba install -c bioconda getorganelle && \
    mamba install -c bioconda fastp && \
    mamba install -c bioconda bam-readcount -m -n bam-readcount && \
    mamba install -c bioconda bowtie2 && \
    mamba install -c bioconda samtools

RUN mamba install -c conda-forge r-base=4.3.0 && \
    mamba install r-remotes

RUN R -e "remotes::install_github('rstudio/renv')"
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

# Install MitoPilot
COPY docker/MitoPilot_*.tar.gz /pkg.tar.gz
RUN R -e 'remotes::install_local("/pkg.tar.gz",upgrade="never")'
RUN rm /pkg.tar.gz

# Default Reference DBs
COPY ref_dbs/getOrganelle /ref_dbs/getOrganelle
