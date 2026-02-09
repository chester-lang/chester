# Chester Documentation System

This directory contains the Chester programming language documentation built with [mdbook](https://rust-lang.github.io/mdBook/) and [mdbook-i18n-helpers](https://github.com/google/mdbook-i18n-helpers) for internationalization.

## Features

- **Multi-language support**: English (default) and Chinese Traditional (zh-TW)
- **Gettext-based translation**: Uses standard PO files for translations
- **Interactive code examples**: Chester syntax highlighting with Ace editor
- **Integrated with website**: Built HTML is served via the Next.js site

## Structure

```
docs/
├── book.toml              # mdbook configuration
├── src/                   # Markdown source files
│   ├── SUMMARY.md        # Table of contents
│   ├── README.md         # Landing page
│   ├── dev/              # Developer documentation
│   └── guide/            # User guide
├── po/                   # Translation files
│   ├── messages.pot      # Generated message catalog (gitignored)
│   └── zh-TW.po         # Chinese Traditional translation
├── theme/                # Custom CSS
├── mode-chester.js       # Ace editor syntax highlighting
└── dev.sh               # Build and translation management script
```

## Prerequisites

Install mdbook and translation tools:

```bash
cargo install mdbook
cargo install mdbook-i18n-helpers
```

## Usage

The `dev.sh` script provides commands for documentation management:

### Development

```bash
# Serve the book locally (English)
./dev.sh serve

# Serve a specific language version
./dev.sh serve zh-TW
```

Visit http://localhost:3000 to view the documentation.

### Building

```bash
# Build English version only
./dev.sh build

# Build a specific language
./dev.sh build zh-TW

# Build all languages
./dev.sh build-all
```

Output is generated in the `book/` directory.

### Translation Workflow

```bash
# 1. Extract translatable strings to POT file
./dev.sh extract

# 2. Initialize a new translation (if needed)
./dev.sh init es  # for Spanish, for example

# 3. Update existing translations after content changes
./dev.sh update

# 4. Edit the PO file (e.g., po/zh-TW.po) with your translations

# 5. Build the translated version
./dev.sh build zh-TW
```

### Normalize PO Files

```bash
# Normalize PO files for consistent formatting
./dev.sh normalize
```

## Integration with Website

The documentation is integrated with the Chester website at `/docs/` route:

1. **Build documentation**: Run `./dev.sh build-all` to generate HTML for all languages
2. **Copy to website**: Use the `scripts/build-docs.sh` script to copy built HTML to `site/public/docs/`
3. **Build website**: The Next.js static export will include the documentation

### NPM Scripts (from site/)

```bash
# Build documentation only
pnpm docs:build

# Serve documentation locally
pnpm docs:serve

# Update translations
pnpm docs:update-translations

# Build everything (docs + website)
pnpm build:full
```

## Adding New Content

1. Create or edit markdown files in `src/`
2. Update `src/SUMMARY.md` to include new pages in the navigation
3. If you want translations, run `./dev.sh update` to extract new strings
4. Edit `po/*.po` files to add translations
5. Build and test: `./dev.sh build-all`

## Translation Status

- **English (en)**: Main language, 100% coverage
- **Chinese Traditional (zh-TW)**: Partial coverage (~314KB PO file)

## Custom Features

### Chester Syntax Highlighting

The `mode-chester.js` file provides Ace editor syntax highlighting for Chester code examples.

### Interactive Editor

Code blocks with the Chester language tag can be made interactive using the `editor.js` configuration.

### Language Picker

The `theme/css/language-picker.css` provides styling for language switcher UI (if implemented).

## Troubleshooting

### Missing Chester.js bundle

The documentation expects a compiled Chester.js bundle at:
- Source: `docs/js/target/scala-*/chester-docs-repl-opt/main.js`
- After build: copied to `book/main.js`

Make sure to build the Chester JS bundle before building docs if you want interactive examples.

### Build errors

If you encounter mdbook build errors:
1. Ensure mdbook and mdbook-i18n-helpers are installed
2. Check that `book.toml` configuration is valid
3. Verify all linked files in `SUMMARY.md` exist

## Resources

- [mdBook User Guide](https://rust-lang.github.io/mdBook/)
- [mdbook-i18n-helpers](https://github.com/google/mdbook-i18n-helpers)
- [gettext documentation](https://www.gnu.org/software/gettext/)
