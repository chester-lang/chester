#!/bin/sh

set -e

# Function to show usage
show_usage() {
    echo "Usage: $0 <command>"
    echo ""
    echo "Commands:"
    echo "  extract     Extract messages to POT file"
    echo "  init <lang> Initialize a new translation for the specified language"
    echo "  update      Extract & update existing translations"
    echo "  build [lang] Build the book for the specified language (or default if not specified)"
    echo "  serve [lang] Serve the book for the specified language (or default if not specified)"
    echo "  normalize   Normalize existing PO files"
    echo "  build-all   Build the book for all languages"
    echo "  summary     Generate summary for the book"
}

# Function to extract messages
extract_messages() {
    MDBOOK_OUTPUT='{"xgettext": {}}' mdbook build -d po
    echo "Messages extracted to po/messages.pot"
}

# Function to initialize a new translation
init_translation() {
    if [ -z "$1" ]; then
        echo "Error: Language code is required"
        exit 1
    fi
    msginit -i po/messages.pot -l "$1" -o "po/$1.po"
    echo "Translation initialized for $1"
}

# Function to update existing translations
update_translations() {
    extract_messages
    for po_file in po/*.po; do
        msgmerge --update "$po_file" po/messages.pot
    done
    echo "Translations updated"
}

CHESTER_JS_PATH="js/target/scala-3.7.4/chester-docs-repl-opt/main.js"
copy_chester_js() {
    cp "$CHESTER_JS_PATH" "$1/main.js"
    cp "$CHESTER_JS_PATH.map" "$1/main.js.map"
}

# Function to build the book for a specific language or default
build_book() {
    if [ -z "$1" ]; then
        mdbook build
        copy_chester_js "book"
        echo "Book built with default language"
    else
        MDBOOK_BOOK__LANGUAGE="$1" mdbook build -d "book/$1"
        copy_chester_js "book/$1"
        echo "Book built for $1"
    fi
}

# Function to serve the book for a specific language or default
serve_book() {
    if [ -z "$1" ]; then
        mdbook serve
    else
        MDBOOK_BOOK__LANGUAGE="$1" mdbook serve -d "book/$1"
    fi
}

# Function to normalize PO files
normalize_po_files() {
    for po_file in po/*.po; do
        mdbook-i18n-normalize "$po_file"
    done
    echo "PO files normalized"
}

# Function to build the book for all languages
build_all_languages() {
    # Build the default (English) version
    mdbook build
    copy_chester_js "book"

    # Get all PO files and build for each language
    for po_file in po/*.po; do
        lang=$(basename "$po_file" .po)
        MDBOOK_BOOK__LANGUAGE="$lang" mdbook build -d "book/$lang"
        copy_chester_js "book/$lang"
        echo "Book built for $lang"
    done

    echo "All language versions built"
}

# Function to generate summary
generate_summary() {
    if ! command -v mdbook-generate-summary &> /dev/null; then
        echo "mdbook-generate-summary is not installed. Installing..."
        cargo install mdbook-generate-summary --version 0.2.0
    fi
    rm -f src/SUMMARY.md
    mdbook-generate-summary src
    echo "Summary generated"
}

# Main script logic
case "$1" in
    extract)
        extract_messages
        ;;
    init)
        init_translation "$2"
        ;;
    update)
        update_translations
        ;;
    build)
        build_book "$2"
        ;;
    serve)
        serve_book "$2"
        ;;
    normalize)
        normalize_po_files
        ;;
    build-all)
        build_all_languages
        ;;
    summary)
        generate_summary
        ;;
    *)
        show_usage
        exit 1
        ;;
esac
