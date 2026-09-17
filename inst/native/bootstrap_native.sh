#!/usr/bin/env bash
# MitoPilot native (no-container) bootstrap. No root required.
# Usage: bash bootstrap_native.sh --prefix DIR [options]
#   --manager micromamba|mamba|conda|pixi   (default: micromamba, downloaded into DIR/bin)
#   --with-optional        also build MitoFinder, ARWEN, ORFfinder
#   --skip-blast-db        do not download the local BLAST database
#   --blast-db-url URL     override the BLAST DB tarball URL
#   --mitopilot-ref REF    GitHub ref to install (default: tag matching this script)
#   --mitopilot-source P   install MitoPilot from a local dir or tarball instead
#   --skip-mitopilot       do not install the MitoPilot R package
#   --dry-run              print the plan, write nothing
# CONDA_CHANNEL_PRIORITY=strict is forced below so conda/mamba/micromamba solve
# with the same priority pixi uses by default.
set -euo pipefail
export CONDA_CHANNEL_PRIORITY=strict

MITOPILOT_VERSION="1.5.5"
BLAST_DB_TAG="blastdb-2026-08-14"
BLAST_DB_URL_DEFAULT="https://github.com/Smithsonian/MitoPilot/releases/download/${BLAST_DB_TAG}/mito_metazoa_blastdb.tar.gz"
ORFFINDER_URL="https://ftp.ncbi.nlm.nih.gov/genomes/TOOLS/ORFfinder/linux-i64/ORFfinder.gz"
MITOFINDER_REPO="https://github.com/RemiAllio/MitoFinder.git"
MICROMAMBA_URL="https://micro.mamba.pm/api/micromamba/linux-64/latest"
PIXI_INSTALL_URL="https://pixi.sh/install.sh"

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
envs_dir="$here/envs"
arwen_src="$here/../../docker/arwen/arwen1.2.3.c"
[ -f "$arwen_src" ] || arwen_src="$here/arwen1.2.3.c"

prefix=""; manager="micromamba"; with_optional=0; skip_blast=0; dry=0
blast_url="$BLAST_DB_URL_DEFAULT"; mp_ref="v${MITOPILOT_VERSION}"; mp_source=""; skip_mp=0

while [ $# -gt 0 ]; do
  case "$1" in
    --prefix) prefix="$2"; shift 2;;
    --manager) manager="$2"; shift 2;;
    --with-optional) with_optional=1; shift;;
    --skip-blast-db) skip_blast=1; shift;;
    --blast-db-url) blast_url="$2"; shift 2;;
    --mitopilot-ref) mp_ref="$2"; shift 2;;
    --mitopilot-source) mp_source="$2"; shift 2;;
    --skip-mitopilot) skip_mp=1; shift;;
    --dry-run) dry=1; shift;;
    -h|--help) sed -n '2,14p' "$0"; exit 0;;
    *) echo "unknown option: $1" >&2; exit 2;;
  esac
done
[ -n "$prefix" ] || { echo "--prefix is required" >&2; exit 2; }
case "$manager" in micromamba|mamba|conda|pixi) ;; *) echo "unknown --manager: $manager" >&2; exit 2;; esac
prefix="$(cd "$(dirname "$prefix")" 2>/dev/null && pwd)/$(basename "$prefix")" || prefix="$prefix"

envs="mitopilot mitos trnascan aragorn bamreadcount"
[ "$with_optional" = 1 ] && envs="$envs orffinder mitofinder"

say() { printf '==> %s\n' "$*"; }
run() { if [ "$dry" = 1 ]; then printf '    [dry-run] %s\n' "$*"; else "$@"; fi; }

say "prefix: $prefix"
say "manager: $manager"
say "envs: $envs"
[ "$with_optional" = 1 ] && say "optional tools: ARWEN, MitoFinder, ORFfinder"
[ "$skip_blast" = 1 ] || say "BLAST DB: $blast_url"
say "activate.sh: $prefix/activate.sh"
if [ "$dry" = 1 ]; then say "dry run, nothing written"; exit 0; fi

mkdir -p "$prefix/bin" "$prefix/envs" "$prefix/opt" "$prefix/nextflow_home"

# 1. package manager -----------------------------------------------------------
case "$manager" in
  micromamba)
    if [ ! -x "$prefix/bin/micromamba" ]; then
      say "downloading micromamba"
      curl -Ls "$MICROMAMBA_URL" | tar -xj -C "$prefix" bin/micromamba
    fi
    export MAMBA_ROOT_PREFIX="$prefix/mamba_root"
    mm="$prefix/bin/micromamba"
    create_env() { "$mm" env create -y -p "$prefix/envs/$1" -f "$envs_dir/$1.yml"; }
    ;;
  mamba|conda)
    command -v "$manager" >/dev/null || { echo "$manager not on PATH" >&2; exit 1; }
    create_env() { "$manager" env create -y -p "$prefix/envs/$1" -f "$envs_dir/$1.yml"; }
    ;;
  pixi)
    if ! command -v pixi >/dev/null && [ ! -x "$prefix/bin/pixi" ]; then
      say "downloading pixi"
      curl -fsSL "$PIXI_INSTALL_URL" | PIXI_HOME="$prefix" PIXI_NO_PATH_UPDATE=1 bash
    fi
    pixi_bin="$(command -v pixi || echo "$prefix/bin/pixi")"
    mkdir -p "$prefix/pixi"
    cp "$here/pixi.toml" "$prefix/pixi/pixi.toml"
    create_env() {
      local e="$1"; [ "$e" = mitopilot ] && e=default
      "$pixi_bin" install --manifest-path "$prefix/pixi/pixi.toml" -e "$e"
      ln -sfn "$prefix/pixi/.pixi/envs/$e" "$prefix/envs/$1"
    }
    ;;
esac

# 2. environments ---------------------------------------------------------------
for e in $envs; do
  if [ -e "$prefix/envs/$e/bin" ]; then say "env $e exists, skipping"; continue; fi
  say "creating env $e"
  create_env "$e"
done
main="$prefix/envs/mitopilot"

# 3. optional tools ------------------------------------------------------------
if [ "$with_optional" = 1 ]; then
  cc="$prefix/envs/mitofinder/bin/cc"; [ -x "$cc" ] || cc="$(ls "$prefix/envs/mitofinder/bin/"*-gcc | head -1)"
  if [ ! -x "$prefix/opt/arwen/arwen" ]; then
    say "building ARWEN"
    mkdir -p "$prefix/opt/arwen"
    "$cc" -O2 -ffast-math -o "$prefix/opt/arwen/arwen" "$arwen_src" -lm
  fi
  if [ ! -f "$prefix/opt/MitoFinder/install.sh.ok" ]; then
    say "installing MitoFinder"
    rm -rf "$prefix/opt/MitoFinder"
    "$prefix/envs/mitofinder/bin/git" clone --depth 1 "$MITOFINDER_REPO" "$prefix/opt/MitoFinder"
    ( cd "$prefix/opt/MitoFinder" && PATH="$prefix/envs/mitofinder/bin:$PATH" ./install.sh )
  fi
  # wrapper pins python2 regardless of which python is first on PATH
  mkdir -p "$prefix/opt/mitofinder_bin"
  cat > "$prefix/opt/mitofinder_bin/mitofinder" <<WRAP
#!/usr/bin/env bash
exec "$prefix/envs/mitofinder/bin/python2" "$prefix/opt/MitoFinder/mitofinder" "\$@"
WRAP
  chmod +x "$prefix/opt/mitofinder_bin/mitofinder"
  if [ ! -x "$prefix/envs/orffinder/bin/ORFfinder" ]; then
    say "installing ORFfinder"
    curl -sSL "$ORFFINDER_URL" | gunzip > "$prefix/envs/orffinder/bin/ORFfinder"
    chmod +x "$prefix/envs/orffinder/bin/ORFfinder"
    "$prefix/envs/orffinder/bin/patchelf" --set-rpath '$ORIGIN/../lib' "$prefix/envs/orffinder/bin/ORFfinder"
  fi
fi

# 4. BLAST DB --------------------------------------------------------------------
if [ "$skip_blast" = 0 ] && [ ! -s "$prefix/ref_dbs/mito_metazoa/taxonomy4blast.sqlite3" ]; then
  say "downloading BLAST DB"
  mkdir -p "$prefix/ref_dbs"
  curl -L "$blast_url" | tar -xz -C "$prefix/ref_dbs"
  BLASTDB="$prefix/ref_dbs/mito_metazoa" "$main/bin/blastdbcmd" -db "$prefix/ref_dbs/mito_metazoa/mito_metazoa" -info >/dev/null
  [ -s "$prefix/ref_dbs/mito_metazoa/taxonomy4blast.sqlite3" ] || { echo "taxonomy4blast.sqlite3 missing" >&2; exit 1; }
fi

# 5. activate.sh -------------------------------------------------------------------
case "$manager" in
  micromamba) act="export MAMBA_ROOT_PREFIX='$prefix/mamba_root'
eval \"\$('$prefix/bin/micromamba' shell hook -s bash)\"
micromamba activate '$main'";;
  mamba|conda) base="$("$manager" info --base)"; act="source '$base/etc/profile.d/conda.sh'
conda activate '$main'";;
  pixi) act="eval \"\$('$pixi_bin' shell-hook --manifest-path '$prefix/pixi/pixi.toml' -e default)\"";;
esac
cat > "$prefix/activate.sh" <<ACT
# MitoPilot $MITOPILOT_VERSION native environment, written by bootstrap_native.sh ($manager)
MITOPILOT_NATIVE_PREFIX='$prefix'
$act
for e in mitos trnascan aragorn bamreadcount orffinder mitofinder; do
  [ -d "\$MITOPILOT_NATIVE_PREFIX/envs/\$e/bin" ] && PATH="\$PATH:\$MITOPILOT_NATIVE_PREFIX/envs/\$e/bin"
done
[ -d "\$MITOPILOT_NATIVE_PREFIX/opt/mitofinder_bin" ] && PATH="\$PATH:\$MITOPILOT_NATIVE_PREFIX/opt/mitofinder_bin"
[ -d "\$MITOPILOT_NATIVE_PREFIX/opt/arwen" ] && PATH="\$PATH:\$MITOPILOT_NATIVE_PREFIX/opt/arwen"
export PATH MITOPILOT_NATIVE_PREFIX
export MITOPILOT_NO_CONDA=1
export NXF_HOME="\${NXF_HOME:-\$MITOPILOT_NATIVE_PREFIX/nextflow_home}"
ACT

# 6. MitoPilot R package ------------------------------------------------------------
if [ "$skip_mp" = 0 ]; then
  say "installing MitoPilot into $main"
  if [ -n "$mp_source" ]; then
    "$main/bin/Rscript" -e "remotes::install_local('$mp_source', upgrade = 'never')"
  else
    "$main/bin/Rscript" -e "BiocManager::install('Smithsonian/MitoPilot@$mp_ref', ask = FALSE, upgrade = 'never')"
  fi
fi

say "done"
echo
echo "Next, in R:"
echo "  MitoPilot::native_check('$prefix')"
echo "  MitoPilot::generate_config(name = 'my_cluster', scheduler = 'slurm', container_engine = 'none', native_prefix = '$prefix')"
