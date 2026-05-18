#!/usr/bin/env bash
#
# Capture --help (and --version) output for each user-arg-taking tool inside
# the MitoPilot Docker image, write the results to inst/help/<tool>.txt for
# bundling with the R package. Re-run whenever the Docker image is rebuilt
# with new tool versions so the in-app help docs stay in sync.
#
# Usage: tools/capture_tool_help.sh [<docker-image>]
#   Defaults to: macguigand/mitopilot:<R-package-version>
#
# Output: inst/help/<tool>.txt — leading "MitoPilot image: ..." + tool version
# header line then the raw --help dump.
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$here/.." && pwd)"
out_dir="$repo_root/inst/help"
mkdir -p "$out_dir"

if [[ "${1-}" == "" ]]; then
    pkg_ver="$(grep -E '^Version:' "$repo_root/DESCRIPTION" | awk '{print $2}')"
    image="macguigand/mitopilot:${pkg_ver}"
else
    image="$1"
fi

echo "Capturing help docs from image: $image" >&2

# Each entry: <out-name>|<conda-env or "">|<help-cmd>|<version-cmd>
# Conda env handles tools installed into named envs (mitos, trnascan-se, aragorn,
# bam-readcount). Empty env runs the cmd directly. Version cmd is best-effort —
# many tools print version with --version, some with -v or in --help output.
TOOLS=(
    "fastp|||fastp --help|fastp --version"
    "getOrganelle|||get_organelle_from_reads.py --help|get_organelle_from_reads.py --version"
    "mitofinder|||mitofinder --help|mitofinder --version"
    "mitos|mitos|runmitos.py --help|runmitos.py --version"
    "trnaScan-SE|trnascan-se|tRNAscan-SE --help|tRNAscan-SE --version"
    "arwen|||arwen -h|arwen -h | head -1"
    "aragorn|aragorn|aragorn -h|aragorn -h | head -1"
    "blastn|||blastn -help|blastn -version"
)

run_in_image() {
    # Runs a shell command inside the image. Conda env optional.
    local env="$1"; shift
    local cmd="$*"
    if [[ -n "$env" ]]; then
        cmd="source activate $env && $cmd"
    fi
    docker run --rm --entrypoint /bin/bash "$image" -c "$cmd" 2>&1 || true
}

for spec in "${TOOLS[@]}"; do
    IFS='|' read -r name env help_cmd version_cmd <<<"$spec"
    out="$out_dir/${name}.txt"
    echo "  $name -> $out" >&2

    version="$(run_in_image "$env" "$version_cmd" | head -3 | tr -d '\r')"
    help="$(run_in_image "$env" "$help_cmd")"

    {
        echo "# MitoPilot image: $image"
        echo "# Tool: $name"
        echo "# Version: $version"
        echo "# Captured: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
        echo "# ----------------------------------------------------------------"
        echo
        echo "$help"
    } > "$out"
done

echo "Done. ${#TOOLS[@]} files written to $out_dir/" >&2
