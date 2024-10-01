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

# Install R
RUN mamba install -c conda-forge r-base=4.3.0 && \
    mamba install r-dplyr && \
    mamba install r-stringr && \
    mamba install r-jsonlite && \
    mamba install r-optparse && \
    mamba install r-tidyr && \
    mamba install r-ggplot2  && \
    mamba install r-readr  && \
    mamba install -c r r-zoo && \
    mamba install -c bioconda bioconductor-biostrings

COPY ref_dbs/getOrganelle /ref_dbs/getOrganelle
