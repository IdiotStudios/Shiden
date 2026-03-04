#!/bin/bash
# Shiden Installation Script for Linux/macOS

set -e

GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

GITHUB_OWNER="IdiotStudios"
GITHUB_REPO="Shiden"
INSTALL_DIR="${INSTALL_DIR:-$HOME/.local/bin}"

# Create install directory if it doesn't exist
mkdir -p "$INSTALL_DIR"

echo -e "${BLUE}Detecting system...${NC}"

# Detect OS and Architecture
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    OS="linux"
    if [[ $(uname -m) == "aarch64" ]]; then
        ARCH="aarch64"
    else
        ARCH="x86_64"
    fi
elif [[ "$OSTYPE" == "darwin"* ]]; then
    OS="macos"
    if [[ $(uname -m) == "arm64" ]]; then
        ARCH="aarch64"
    else
        ARCH="x86_64"
    fi
else
    echo -e "${RED}Unsupported OS: $OSTYPE${NC}"
    exit 1
fi

BINARY_NAME="shiden"
ASSET_NAME="shiden-${OS}-${ARCH}"

echo -e "${BLUE}OS: $OS, Architecture: $ARCH${NC}"
echo -e "${BLUE}Fetching latest release...${NC}"

# Get the latest release
LATEST_RELEASE=$(curl -s "https://api.github.com/repos/$GITHUB_OWNER/$GITHUB_REPO/releases/latest")
TAG=$(echo "$LATEST_RELEASE" | grep '"tag_name"' | head -1 | cut -d'"' -f4)
DOWNLOAD_URL="https://github.com/$GITHUB_OWNER/$GITHUB_REPO/releases/download/$TAG/$ASSET_NAME"

if [ -z "$TAG" ]; then
    echo -e "${RED}Failed to fetch latest release${NC}"
    exit 1
fi

echo -e "${GREEN}Latest version: $TAG${NC}"
echo -e "${BLUE}Downloading binary...${NC}"

# Download the binary
TMP_FILE="/tmp/$ASSET_NAME.$$.tmp"
if ! curl -L --progress-bar "$DOWNLOAD_URL" -o "$TMP_FILE"; then
    echo -e "${RED}Failed to download binary from $DOWNLOAD_URL${NC}"
    rm -f "$TMP_FILE"
    exit 1
fi

# Verify checksum if available
CHECKSUM_URL="https://github.com/$GITHUB_OWNER/$GITHUB_REPO/releases/download/$TAG/$ASSET_NAME.sha256"
CHECKSUM_FILE="/tmp/$ASSET_NAME.sha256.$$.tmp"

if curl -s -L -f -o "$CHECKSUM_FILE" "$CHECKSUM_URL"; then
    echo -e "${BLUE}Verifying checksum...${NC}"
    
    EXPECTED_HASH=$(head -1 "$CHECKSUM_FILE" | awk '{print $1}')
    
    if [ -z "$EXPECTED_HASH" ]; then
        echo -e "${RED}Error: Could not read checksum from file${NC}"
        rm -f "$TMP_FILE" "$CHECKSUM_FILE"
        exit 1
    fi
    
    # Compute the actual hash
    if command -v sha256sum &> /dev/null; then
        ACTUAL_HASH=$(sha256sum "$TMP_FILE" | awk '{print $1}')
    elif command -v shasum &> /dev/null; then
        ACTUAL_HASH=$(shasum -a 256 "$TMP_FILE" | awk '{print $1}')
    else
        echo -e "${RED}No checksum command available (sha256sum or shasum)${NC}"
        rm -f "$TMP_FILE" "$CHECKSUM_FILE"
        exit 1
    fi
    
    # Compare hashes
    if [ "$EXPECTED_HASH" != "$ACTUAL_HASH" ]; then
        echo -e "${RED}Checksum verification failed!${NC}"
        echo -e "${RED}Expected: $EXPECTED_HASH${NC}"
        echo -e "${RED}Got:      $ACTUAL_HASH${NC}"
        rm -f "$TMP_FILE" "$CHECKSUM_FILE"
        exit 1
    fi
    
    rm -f "$CHECKSUM_FILE"
    echo -e "${GREEN}Checksum verified${NC}"
else
    echo -e "${BLUE}No checksum file available, skipping verification${NC}"
fi

# Move binary to install directory
mv "$TMP_FILE" "$INSTALL_DIR/$BINARY_NAME"
chmod +x "$INSTALL_DIR/$BINARY_NAME"

echo -e "${GREEN}✓ Installation complete!${NC}"
echo -e "${BLUE}Binary installed to: $INSTALL_DIR/$BINARY_NAME${NC}"

# Check if install directory is in PATH
if [[ ":$PATH:" == *":$INSTALL_DIR:"* ]]; then
    echo -e "${GREEN}✓ $INSTALL_DIR is in your PATH${NC}"
else
    echo -e "${BLUE}Note: Add $INSTALL_DIR to your PATH to use shiden from anywhere${NC}"
    echo -e "${BLUE}Add this line to your shell profile (~/.bashrc, ~/.zshrc, etc):${NC}"
    echo -e "${BLUE}export PATH=\"$INSTALL_DIR:\$PATH\"${NC}"
fi

echo -e ""
echo -e "${BLUE}To update in the future, run: ${GREEN}shiden update${NC}"
