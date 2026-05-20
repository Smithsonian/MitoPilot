# Bundled tool help docs

Each `<tool>.txt` here is a captured `--help` dump from the MitoPilot Docker
image, shown in the app when the user clicks the `?` icon next to that tool's
options field.

To refresh after a Docker image rebuild:

    tools/capture_tool_help.sh                  # uses macguigand/mitopilot:<DESCRIPTION Version>
    tools/capture_tool_help.sh my/image:tag     # custom image

Commit the regenerated `.txt` files. The first comment block in each file
records the source image + tool version + capture timestamp.

If a tool's `.txt` is missing the app falls back to a generic
"docs not yet captured" message.
