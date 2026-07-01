#!/bin/sh

set -e

show_usage() {
    echo "Usage: $0 <command>"
    echo ""
    echo "Commands:"
    echo "  extract     Extract messages to POT file"
    echo "  init <lang> Initialize a new translation for the specified language"
    echo "  update      Extract & update existing translations"
    echo "  build [lang] Build the book for the specified language (or default if not specified)"
    echo "  serve [lang] Serve the book for the specified language (or default if not specified)"
    echo "  build-all   Build the book for all languages"
}

extract_messages() {
    MDBOOK_OUTPUT='{"xgettext": {}}' mdbook build -d po
    echo "Messages extracted to po/messages.pot"
}

init_translation() {
    if [ -z "$1" ]; then
        echo "Error: Language code is required"
        exit 1
    fi
    msginit -i po/messages.pot -l "$1" -o "po/$1.po"
    echo "Translation initialized for $1"
}

update_translations() {
    extract_messages
    for po_file in po/*.po; do
        if [ -f "$po_file" ]; then
            msgmerge --update "$po_file" po/messages.pot
        fi
    done
    echo "Translations updated"
}

build_book() {
    if [ -z "$1" ]; then
        mdbook build
        echo "Book built with default language"
    else
        MDBOOK_BOOK__LANGUAGE="$1" mdbook build -d "book/$1"
        echo "Book built for $1"
    fi
}

serve_book() {
    if [ -z "$1" ]; then
        mdbook serve
    else
        MDBOOK_BOOK__LANGUAGE="$1" mdbook serve -d "book/$1"
    fi
}

build_all_languages() {
    mdbook build
    for po_file in po/*.po; do
        if [ -f "$po_file" ]; then
            lang=$(basename "$po_file" .po)
            MDBOOK_BOOK__LANGUAGE="$lang" mdbook build -d "book/$lang"
            echo "Book built for $lang"
        fi
    done
    echo "All language versions built"
}

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
    build-all)
        build_all_languages
        ;;
    *)
        show_usage
        exit 1
        ;;
esac
