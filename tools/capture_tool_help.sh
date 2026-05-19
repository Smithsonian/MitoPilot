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

# Each entry: <out-name>||<help-cmd>|<version-cmd>     (3 separators, 4 fields)
# Tools installed into a named conda env (mitos, trnascan-se, aragorn) are
# invoked through their absolute path in /opt/conda/envs/<env>/bin so we don't
# depend on `conda activate` working in a non-interactive bash. Version cmd is
# best-effort — set to `true` when the tool has no --version flag.
TOOLS=(
    "fastp||fastp --help|fastp --version"
    "getOrganelle||get_organelle_from_reads.py --help|get_organelle_from_reads.py --version"
    "mitofinder||mitofinder -h|true"
    "mitos||/opt/conda/envs/mitos/bin/runmitos --help|/opt/conda/envs/mitos/bin/runmitos --version"
    "trnaScan-SE||/opt/conda/envs/trnascan-se/bin/tRNAscan-SE --help|/opt/conda/envs/trnascan-se/bin/tRNAscan-SE --version"
    "arwen||arwen -h|true"
    "aragorn||/opt/conda/envs/aragorn/bin/aragorn -h|true"
    "blastn||blastn -help|blastn -version"
)

run_in_image() {
    # Runs a shell command inside the image (always via /bin/bash so pipes work).
    local cmd="$*"
    docker run --rm --entrypoint /bin/bash "$image" -c "$cmd" 2>&1 || true
}

for spec in "${TOOLS[@]}"; do
    IFS='|' read -r name _env help_cmd version_cmd <<<"$spec"
    out="$out_dir/${name}.txt"
    echo "  $name -> $out" >&2

    version="$(run_in_image "$version_cmd" | head -1 | tr -d '\r')"
    if [[ "$version" == *"not found"* || "$version" == *"unrecognized"* || -z "$version" ]]; then
        version="(version not detected)"
    fi
    help="$(run_in_image "$help_cmd")"

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
