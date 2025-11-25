# Archive

This directory contains deprecated/archived code that is kept for reference but is no longer part of the active codebase.

## Contents

### zyn_parser

**Archived:** November 2024

The original hand-written parser for the Zyn language. This has been superseded by `zyn_peg`, which uses a PEG grammar approach with JSON semantic actions to generate TypedAST directly at parse time.

The new approach (`crates/zyn_peg`) is:
- More maintainable (grammar + actions in a single `.zyn` file)
- More flexible (easy to add new language frontends)
- Better documented (see `book/` directory)

If you need to reference the old implementation, it's preserved here.
