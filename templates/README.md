# File Templates

Templates consumed by `doom/.config/doom/modules/files.el` via the `+tim/new-file` command (interactive: language → kind → filename).

## Format

`templates.conf` registers each template, one per line:

```
language|extension|kind|template_path|description
```

Example:

```
php|php|class|php/class.php|PHP Class
```

The `template_path` is relative to `templates/`.

## Placeholders

Expanded by `+tim/templates--expand-placeholders` at file-creation time:

| Token                | Value                                            |
| -------------------- | ------------------------------------------------ |
| `{{CLASS_NAME}}`     | filename → PascalCase                            |
| `{{INTERFACE_NAME}}` | same as `{{CLASS_NAME}}`                         |
| `{{COMPONENT_NAME}}` | filename → kebab-case                            |
| `{{SELECTOR}}`       | same as `{{COMPONENT_NAME}}`                     |
| `{{MODULE_NAME}}`    | filename without extension                       |
| `{{FILE_NAME}}`      | filename without extension                       |
| `{{TITLE}}`          | filename → PascalCase                            |
| `{{NAMESPACE}}`      | inferred from path (PHP `\\`, JVM `.`)           |
| `{{HEADER_GUARD}}`   | filename uppercased, hyphens → underscores       |
| `{{DATE}}`           | current date (`YYYY-MM-DD`)                      |

## Adding a template

1. Drop the template file under `templates/<lang>/`.
2. Add a row to `templates.conf`.
3. Use `M-x +tim/new-file` in doom.

The path is resolved via `~/dotfiles/templates/` (hardcoded in `files.el`), so the dir is not stowed.
