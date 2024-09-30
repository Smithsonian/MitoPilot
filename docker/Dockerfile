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
    mamba install -c bioconda fastp

COPY ref_dbs/getOrganelle /ref_dbs/getOrganelle
