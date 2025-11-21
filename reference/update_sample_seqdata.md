# Update sequence files for existing project

Update the sequence data files for samples in an existing project
database. After updating, user should rerun these samples, starting with
the ASSEMBLE module. Creates a backup of the existing database prior to
updating.

## Usage

``` r
update_sample_seqdata(path = ".", update_mapping_fn = NULL, mapping_id = "ID")
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- update_mapping_fn:

  Path to the update mapping CSV file. Must contain columns "ID", "R1",
  and "R2"

- mapping_id:

  Column name of the update mapping file to use as the primary key
