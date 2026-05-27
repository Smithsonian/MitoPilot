# MitoPilot user config directory

Persistent, per-user directory where saved cluster profiles
(\`config.\<name\>\` files) are stored. Created on demand by
\[generate_config()\]. Uses \[tools::R_user_dir()\] so it follows the
platform convention (e.g. \`~/.local/share/MitoPilot\` or
\`~/.config/MitoPilot\`).

## Usage

``` r
mitopilot_config_dir()
```

## Value

Path to the MitoPilot config directory (not guaranteed to exist).
