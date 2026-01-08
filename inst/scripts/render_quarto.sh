#!/bin/bash
# Render Quarto documentation site
# Usage: ./inst/scripts/render_quarto.sh [--fresh]
#
# Options:
#   --fresh    Clear cache (_freeze and _site) before rendering
#   --help     Show this help message

set -e

# Configuration
QUARTO_EXE="/mnt/c/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"
QUARTO_DIR="quarto"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Parse arguments
FRESH=false
for arg in "$@"; do
    case $arg in
        --fresh)
            FRESH=true
            shift
            ;;
        --help|-h)
            echo "Usage: ./inst/scripts/render_quarto.sh [--fresh]"
            echo ""
            echo "Options:"
            echo "  --fresh    Clear cache (_freeze and _site) before rendering"
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

# Clear cache if --fresh
if [ "$FRESH" = true ]; then
    echo -e "${YELLOW}Clearing Quarto cache...${NC}"
    rm -rf "$QUARTO_DIR/_freeze" "$QUARTO_DIR/_site"
    echo -e "${GREEN}Cache cleared${NC}"
fi

# Render
echo -e "${YELLOW}Rendering Quarto site...${NC}"
"$QUARTO_EXE" render "$QUARTO_DIR"

echo -e "${GREEN}Done! Output at: $QUARTO_DIR/_site/index.html${NC}"
