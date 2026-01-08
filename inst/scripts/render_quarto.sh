#!/bin/bash
# Render Quarto documentation site
# Usage: bash inst/scripts/render_quarto.sh [--cached]
#
# By default, clears cache (_freeze and _site) before rendering.
# Use --cached to skip cache clearing and use existing freeze files.

set -e

# Configuration
QUARTO_EXE="/mnt/c/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"
QUARTO_DIR="quarto"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default: fresh render (clear cache)
FRESH=true

# Parse arguments
for arg in "$@"; do
    case $arg in
        --cached)
            FRESH=false
            shift
            ;;
        --help|-h)
            echo "Usage: bash inst/scripts/render_quarto.sh [--cached]"
            echo ""
            echo "By default, clears cache and renders fresh."
            echo ""
            echo "Options:"
            echo "  --cached   Skip cache clearing, use existing _freeze files"
            echo "  --help     Show this help message"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $arg${NC}"
            exit 1
            ;;
    esac
done

# Check Quarto exists
if [ ! -f "$QUARTO_EXE" ]; then
    echo -e "${RED}Error: Quarto not found at $QUARTO_EXE${NC}"
    echo "Please update QUARTO_EXE path in this script"
    exit 1
fi

# Check we're in the right directory
if [ ! -d "$QUARTO_DIR" ]; then
    echo -e "${RED}Error: $QUARTO_DIR directory not found${NC}"
    echo "Please run this script from the package root directory"
    exit 1
fi

# Clear cache by default
if [ "$FRESH" = true ]; then
    echo -e "${YELLOW}Clearing Quarto cache...${NC}"
    rm -rf "$QUARTO_DIR/_freeze" "$QUARTO_DIR/_site"
    echo -e "${GREEN}Cache cleared${NC}"
else
    echo -e "${YELLOW}Using cached freeze files...${NC}"
fi

# Render
echo -e "${YELLOW}Rendering Quarto site...${NC}"
"$QUARTO_EXE" render "$QUARTO_DIR"

echo -e "${GREEN}Done! Output at: $QUARTO_DIR/_site/index.html${NC}"
