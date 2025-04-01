FROM condaforge/mambaforge:24.9.2-0

RUN apt-get update && \
    apt-get install -y jq && \
    apt-get install -y parallel && \
    rm -rf /var/lib/apt/lists/*

# Install Conda packages
RUN conda config --set channel_priority true && \ 
    conda config --add channels defaults && \
    conda config --add channels conda-forge && \
    conda config --add channels bioconda && \
    mamba install -c bioconda fastp=0.23.4 && \
    mamba install -c bioconda getorganelle=1.7.7.1 && \
    mamba install -c bioconda bam-readcount=1.0.1 -m -n bam-readcount && \
    mamba install -c bioconda bowtie2=2.5.4 && \
    mamba install -c bioconda samtools=1.21 && \
    mamba install -c bioconda trnascan-se=2.0.12 -m -n trnascan-se && \
    mamba install -c bioconda mitos=2.1.9 -m -n mitos && \
    mamba install -c conda-forge r-base=4.4.2 && \
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
COPY ref_dbs/Mitos2 /ref_dbs/Mitos2
