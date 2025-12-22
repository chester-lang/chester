#!/bin/bash
# This script builds the mdbook documentation and copies it to the Next.js public directory
# so it can be served as static files

set -e

cd "$(dirname "$0")/.."

echo "Building mdbook documentation..."

# Build English version
cd docs
./dev.sh build

# Build all translations
./dev.sh build zh-TW

echo "Copying documentation to Next.js public directory..."

# Create docs directory in public if it doesn't exist
mkdir -p site/public/docs

# Copy the built documentation
# The English version goes to /docs/
cp -r book/* site/public/docs/

echo "✓ Documentation built and copied to site/public/docs/"
echo ""
echo "To preview:"
echo "  cd site && pnpm dev"
echo "  Then visit http://localhost:3000/docs/"
