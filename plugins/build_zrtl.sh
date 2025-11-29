#!/bin/bash
#
# ZRTL Plugin Build Script
#
# Builds all ZRTL plugins and generates .zrtl files (native dynamic libraries)
# for runtime loading by the Zyntax compiler.
#
# Plugins are automatically discovered from the workspace Cargo.toml.
#
# Usage:
#   ./build_zrtl.sh              # Build debug
#   ./build_zrtl.sh --release    # Build release
#   ./build_zrtl.sh --output dir # Specify output directory

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLUGINS_DIR="$SCRIPT_DIR"
cd "$PLUGINS_DIR"

# Default values
PROFILE="debug"
OUTPUT_DIR="$SCRIPT_DIR/target/zrtl"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --release)
            PROFILE="release"
            shift
            ;;
        --output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [--release] [--output <dir>]"
            echo ""
            echo "Options:"
            echo "  --release      Build in release mode"
            echo "  --output dir   Output directory (default: target/zrtl)"
            echo ""
            echo "Plugins are auto-discovered from subdirectories"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Discover plugins - subdirectories in the plugins directory with a Cargo.toml
discover_plugins() {
    for dir in "$PLUGINS_DIR"/*/; do
        dir="${dir%/}"
        name="$(basename "$dir")"
        if [ -f "$dir/Cargo.toml" ]; then
            echo "$name"
        fi
    done | sort
}

# Get version from a plugin's Cargo.toml
get_plugin_version() {
    local plugin=$1
    grep -E '^version\s*=' "$PLUGINS_DIR/$plugin/Cargo.toml" 2>/dev/null | head -1 | sed 's/.*"\([^"]*\)".*/\1/'
}

# Platform-specific library extension
get_dylib_ext() {
    case "$(uname -s)" in
        Darwin*)  echo "dylib" ;;
        MINGW*|MSYS*|CYGWIN*) echo "dll" ;;
        *)        echo "so" ;;
    esac
}

get_lib_prefix() {
    case "$(uname -s)" in
        MINGW*|MSYS*|CYGWIN*) echo "" ;;
        *) echo "lib" ;;
    esac
}

DYLIB_EXT=$(get_dylib_ext)
LIB_PREFIX=$(get_lib_prefix)

# Discover plugins
PLUGINS=($(discover_plugins))

if [ ${#PLUGINS[@]} -eq 0 ]; then
    echo "Error: No plugins found"
    exit 1
fi

echo "======================================"
echo "ZRTL Plugin Builder"
echo "======================================"
echo "Profile: $PROFILE"
echo "Output:  $OUTPUT_DIR"
echo "Discovered ${#PLUGINS[@]} plugins: ${PLUGINS[*]}"
echo ""

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Build flags
BUILD_FLAGS=""
if [ "$PROFILE" = "release" ]; then
    BUILD_FLAGS="--release"
fi

# Build each plugin
for plugin in "${PLUGINS[@]}"; do
    echo "Building $plugin..."
    cargo build -p "$plugin" $BUILD_FLAGS

    # Source path
    src="$SCRIPT_DIR/target/$PROFILE/${LIB_PREFIX}${plugin}.${DYLIB_EXT}"

    # Destination path
    dst="$OUTPUT_DIR/${plugin}.zrtl"

    if [ -f "$src" ]; then
        cp "$src" "$dst"
        size=$(ls -lh "$dst" | awk '{print $5}')
        echo "  -> $dst ($size)"
    else
        echo "  Warning: $src not found"
    fi
done

# Generate manifest
MANIFEST="$OUTPUT_DIR/plugins.json"
echo ""
echo "Generating manifest..."

# Get platform info
PLATFORM=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

# Start JSON
cat > "$MANIFEST" << EOF
{
  "version": "1.0.0",
  "platform": "$PLATFORM",
  "arch": "$ARCH",
  "profile": "$PROFILE",
  "plugins": [
EOF

first=true
for plugin in "${PLUGINS[@]}"; do
    zrtl_file="$OUTPUT_DIR/${plugin}.zrtl"
    if [ -f "$zrtl_file" ]; then
        size=$(stat -f%z "$zrtl_file" 2>/dev/null || stat -c%s "$zrtl_file" 2>/dev/null || echo "0")
        version=$(get_plugin_version "$plugin")

        if [ "$first" = true ]; then
            first=false
        else
            echo "," >> "$MANIFEST"
        fi

        cat >> "$MANIFEST" << EOF
    {
      "name": "$plugin",
      "version": "$version",
      "file": "${plugin}.zrtl",
      "size": $size
    }
EOF
    fi
done

cat >> "$MANIFEST" << 'EOF'
  ]
}
EOF

echo ""
echo "======================================"
echo "Build complete!"
echo "======================================"
echo "Output directory: $OUTPUT_DIR"
echo ""
ls -la "$OUTPUT_DIR"
