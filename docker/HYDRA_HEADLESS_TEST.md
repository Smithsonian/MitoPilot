# Headless MitoPilot test run on Smithsonian Hydra

Run the MitoPilot GUI headless on a Hydra node, reach it from your laptop over an
SSH tunnel, build a cluster submission script in the GUI, and submit the pipeline
to SGE. The container's bundled R is used for both project setup and the GUI, so
no separate R install is needed.

Image: `macguigand/mitopilot:headless-gui` (Singularity + SGE; published to Docker Hub).

## Step 0 (local, one time): publish the image

Hydra pulls the image from Docker Hub via Singularity, so a local-only build is
not enough.

```bash
docker push macguigand/mitopilot:headless-gui
# or: ./docker/deploy-dockerhub.sh headless-gui   # rebuilds + pushes, prompts for creds
```

## On Hydra

Hydra = Singularity engine + SGE scheduler. Plan: use the container's bundled R
to create the project and run the headless GUI; tunnel a browser to it; build the
submission script in the GUI; submit it to SGE.

Replace `<user>` and paths with your own writable space.

### 1. Get an interactive node (do not compute on the login node)

```bash
qrsh -pe mthread 2 -l mres=4G,h_data=4G,h_vmem=4G
```

### 2. Cache the image as a .sif

```bash
cd /pool/genomics/<user>
export SINGULARITY_CACHEDIR=$PWD/.sing_cache
export NXF_SINGULARITY_CACHEDIR=$PWD/.sing_cache   # Nextflow reuses the same image
singularity pull mitopilot-headless.sif docker://macguigand/mitopilot:headless-gui
```

### 3. Prepare the test project with the bundled R

Use **absolute paths** and pin the in-container working dir with `--pwd`.
`singularity exec` otherwise defaults the working directory to `$HOME` (not your
shell's cwd), so a relative `path=` lands in the wrong place.

```bash
cd /pool/genomics/<user>/headless_test
singularity exec --bind /pool,/scratch --pwd $PWD mitopilot-headless.sif R
```

```r
proj <- "/pool/genomics/<user>/headless_test/hydra_headless_test"
MitoPilot::new_test_project(
  path      = proj,
  n         = 2,                                   # small / fast
  executor  = "NMNH_Hydra",
  container = "macguigand/mitopilot:headless-gui", # pipeline jobs use the same image
  Rproj     = FALSE
)
q(save = "no")
```

### 4. Launch the headless GUI from the project dir (bundled R)

```bash
proj=/pool/genomics/<user>/headless_test/hydra_headless_test
singularity exec --bind /pool,/scratch --pwd "$proj" mitopilot-headless.sif \
  R -e "setwd('$proj'); MitoPilot::MitoPilot(host='0.0.0.0', port=3838, launch.browser=FALSE)"
```

Confirm the project dir has `.config` and a `.sqlite` (`list.files(all.files = TRUE)`)
before launching.

This prints an `ssh -L 3838:<node>:3838 <user>@hydra-login...` line. Keep this
session running (it is the Shiny server).

### 5. From your laptop

Run the printed `ssh -L ...` command, then open <http://localhost:3838>.

## In the GUI (headless behavior)

- "Run from App" is **disabled** on purpose (never run Nextflow on the node where
  the GUI lives).
- Click a workflow run. The modal shows an **editable submission script**, already
  populated with `#$` SGE directives derived from the project `.config`.
- Edit the environment-setup block for Hydra, e.g.:

  ```sh
  source ~/.bashrc
  module load tools/java/21.0.2
  export NXF_SINGULARITY_CACHEDIR=/pool/genomics/<user>/.sing_cache
  ```

- Then either:
  - **"Submit to Cluster (qsub)"** if `qsub` is on PATH inside the container, or
  - **"Save Script Only"** (most reliable on Hydra) and submit from the qrsh shell:
    `qsub <mode>_<timestamp>.sh`

Edits to the resource / environment block are saved to
`.mitopilot_submit.template` in the project directory, so the next run pre-fills
them.

## Notes / caveats

- **`qsub` inside the container.** Singularity shadows the host `/usr/bin`, so
  `qsub` may not be visible inside the container unless `/cm/shared` is bound and
  `SGE_ROOT` is set. For testing, prefer **"Save Script Only" + `qsub` from the
  qrsh shell**.
- The submitted `.sh` runs **natively** on the SGE node (not in the container) and
  calls `nextflow`, which then uses Singularity per process. That node therefore
  needs `java` + `nextflow` on PATH, which the environment-setup block handles.
- The GUI is for sample tables, annotation editing, and exports while headless;
  the pipeline always runs as a scheduled SGE job, never from the app.
