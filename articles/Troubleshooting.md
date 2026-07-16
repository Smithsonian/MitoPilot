# Troubleshooting

This page collects common error messages and how to resolve them. For
general usage questions, see the
[FAQ](https://smithsonian.github.io/MitoPilot/articles/FAQ.md).

## Nextflow process exit codes

When a step fails, Nextflow reports the process and an exit status, for
example:

    Caused by:
      Process `WF2:ANNOTATE:annotate (SAMPLE_ID)` terminated with an error exit status (140)

The number identifies the cause. Nextflow retries a failed process
automatically (up to a few times), and each retry requests more memory
(the requested memory is multiplied by the attempt number), so a step
that fails once often succeeds on retry. A process that fails on every
retry needs your attention.

### “exit status (137)”

Exit 137 means the process was killed with `SIGKILL` (137 = 128 + 9).
The usual cause is running out of memory: the container or the cluster
scheduler enforces a memory limit, and a process that exceeds it is
killed outright rather than allowed to finish. It can also appear when a
job hits a scheduler time limit, or when a container is stopped
manually.

Because each retry requests more memory, a one-off 137 that then
succeeds needs no action. If a step fails with 137 on every retry,
increase its requested memory in the R Shiny GUI for the process that
failed, then rerun the workflow (the same fix as `exit status (140)`
below).

### “exit status (140)”

If your Nextflow log contains an error that looks something like this:

    Caused by:
      Process `WF2:ANNOTATE:annotate (SAMPLE_ID)` terminated with an error exit status (140)

Usually this indicates that the process ran out of memory (RAM).
Sometimes this will happen for all samples, other times only certain
difficult samples will need more RAM. Try using the R Shiny GUI to
increase the requested memory for the process that failed, then rerun
the workflow.

### “exit status (127)”

This indicates that MitoPilot was unable to find a file, directory, or
executable. Double check that the paths you specified when creating your
project are correct. Also make sure you’re using the correct execution
environment. This error is commonly caused by using the “local” executor
when running MitoPilot on a computing cluster.

### “exit status (255)”

Often this indicates that one or more of your input FASTQ files could
not be found. Double check the R1 and R2 columns of your map file and
make sure all of the `data_path` contains all of your sequence read
files.

## Other errors

### “database disk image is malformed”

You may encounter an error message in your R Studio console that looks
something like this:

``` r
Listening on http://127.0.0.1:4329
Warning: Couldn't set synchronous mode: database disk image is malformed
Use `synchronous` = NULL to turn off this warning.
Database attached: /pool/public/genomics/macguigand/MitoPilot/testing/turtle_test/run_01/.sqlite
Warning: Error in db_query_fields.DBIConnection: Can't query fields.
ℹ Using SQL: SELECT * FROM `pre_opts` AS `q02` WHERE (0 = 1)
Caused by error:
! database disk image is malformed
...
```

This indicates that your `.sqlite` database is corrupted. The exact
cause of this error can be difficult to determine. Fortunately, you may
be able to recover your database by running the following commands in
your project directory.

``` bash
mv .sqlite .sqlite_corrupted
sqlite3 .sqlite_corrupted ".recover" | sqlite3 .sqlite
```

Then try relaunching the MitoPilot app.

### “Insufficient memory for the Java Runtime Environment”

This suggests that the Nextflow head process (which controls all other
jobs) is running out of Java heap space. To fix, run the following
command in the console or add it to your submission submission script
before launching Nextflow.

Alternatively, submit the Nextflow job with fewer samples at time.

``` bash
export NXF_OPTS="-Xms500m -Xmx8g"
```
