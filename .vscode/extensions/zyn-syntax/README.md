# Zyn Grammar Syntax Highlighting

VSCode extension for syntax highlighting of `.zyn` grammar files.

## Installation

### Option 1: Symlink to extensions folder

```bash
ln -s "$(pwd)/.vscode/extensions/zyn-syntax" ~/.vscode/extensions/zyn-syntax
```

Then reload VSCode.

### Option 2: Install via VSIX

```bash
cd .vscode/extensions/zyn-syntax
npx vsce package
code --install-extension zyn-syntax-0.1.0.vsix
```

## Features

- Syntax highlighting for PEG grammar rules
- Highlighting for semantic action blocks (JSON)
- Special highlighting for:
  - Rule definitions and references
  - JSON command keys (`define`, `get_child`, etc.)
  - Positional arguments (`$1`, `$2`, etc.)
  - Built-in tokens (`SOI`, `EOI`, `ANY`, etc.)
  - Operators (`~`, `|`, `*`, `+`, `?`, etc.)
