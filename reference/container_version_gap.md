# Compare a project's configured container against the installed package version

The pipeline's behaviour lives in the image, not in the R package, so a
project pinned to an older image silently runs older code: the local
BLAST database, for instance, only exists from 1.5.2 onward, and a
project pointing at an earlier tag sends every sample to the remote
search instead with no visible sign. A custom container is left alone
deliberately, mirroring \`migrate_config()\`: it has no version to
compare against.

## Usage

``` r
container_version_gap(path)
```

## Arguments

- path:

  Project directory containing \`.config\`.

## Value

NULL when the container matches, is custom, or cannot be read. Otherwise
a list with \`configured\` and \`expected\` image references.
