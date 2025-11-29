#!/bin/bash
#
# ZRTL Static Library Build Script
#
# Builds architecture-dependent static libraries (.a files) for AOT static linking.
# Supports cross-compilation for multiple architectures.
#
# Plugins are automatically discovered from the workspace Cargo.toml.
#
# Usage:
#   ./build_static.sh                    # Build for host architecture
#   ./build_static.sh --all              # Build for all supported targets
#   ./build_static.sh --target aarch64-apple-darwin
#   ./build_static.sh --release --output lib/

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLUGINS_DIR="$SCRIPT_DIR"
cd "$PLUGINS_DIR"

# Default values
PROFILE="release"  # Static libs are usually release
OUTPUT_DIR="$SCRIPT_DIR/target/static"
BUILD_ALL=false
TARGETS=()

# Supported targets for cross-compilation
SUPPORTED_TARGETS=(
    # macOS
    "x86_64-apple-darwin"
    "aarch64-apple-darwin"
    # Linux
    "x86_64-unknown-linux-gnu"
    "aarch64-unknown-linux-gnu"
    "x86_64-unknown-linux-musl"
    "aarch64-unknown-linux-musl"
    # Windows
    "x86_64-pc-windows-gnu"
    "x86_64-pc-windows-msvc"
)

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --release)
            PROFILE="release"
            shift
            ;;
        --debug)
            PROFILE="debug"
            shift
            ;;
        --output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --target)
            TARGETS+=("$2")
            shift 2
            ;;
        --all)
            BUILD_ALL=true
            shift
            ;;
        --list-targets)
            echo "Supported targets:"
            for t in "${SUPPORTED_TARGETS[@]}"; do
                echo "  $t"
            done
            exit 0
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --release          Build in release mode (default)"
            echo "  --debug            Build in debug mode"
            echo "  --output dir       Output directory (default: target/static)"
            echo "  --target TARGET    Build for specific target (can be repeated)"
            echo "  --all              Build for all supported targets"
            echo "  --list-targets     List all supported cross-compilation targets"
            echo ""
            echo "Plugins are auto-discovered from subdirectories"
            echo ""
            echo "Examples:"
            echo "  $0                                        # Build for host"
            echo "  $0 --target aarch64-apple-darwin          # Build for Apple Silicon"
            echo "  $0 --target x86_64-unknown-linux-musl     # Build for Linux musl"
            echo "  $0 --all                                  # Build all targets"
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

# Discover plugins
PLUGINS=($(discover_plugins))

if [ ${#PLUGINS[@]} -eq 0 ]; then
    echo "Error: No plugins found"
    exit 1
fi

# If no targets specified and not building all, use host target
if [ ${#TARGETS[@]} -eq 0 ] && [ "$BUILD_ALL" = false ]; then
    HOST_TARGET=$(rustc -vV | grep host | cut -d' ' -f2)
    TARGETS=("$HOST_TARGET")
fi

# If building all, use supported targets
if [ "$BUILD_ALL" = true ]; then
    TARGETS=("${SUPPORTED_TARGETS[@]}")
fi

echo "======================================"
echo "ZRTL Static Library Builder"
echo "======================================"
echo "Profile: $PROFILE"
echo "Output:  $OUTPUT_DIR"
echo "Discovered ${#PLUGINS[@]} plugins: ${PLUGINS[*]}"
echo "Targets: ${TARGETS[*]}"
echo ""

# Create base output directory
mkdir -p "$OUTPUT_DIR"

# Build flags
BUILD_FLAGS=""
if [ "$PROFILE" = "release" ]; then
    BUILD_FLAGS="--release"
fi

# Function to check if target is installed
check_target() {
    local target=$1
    if ! rustup target list --installed | grep -q "^$target$"; then
        echo "  Installing target: $target"
        rustup target add "$target" 2>/dev/null || {
            echo "  Warning: Failed to install target $target, skipping..."
            return 1
        }
    fi
    return 0
}

# Function to get static lib name
get_staticlib_name() {
    local plugin=$1
    local target=$2

    case "$target" in
        *-windows-*)
            echo "${plugin}.lib"
            ;;
        *)
            echo "lib${plugin}.a"
            ;;
    esac
}

# Build for each target
for target in "${TARGETS[@]}"; do
    echo "======================================"
    echo "Building for: $target"
    echo "======================================"

    # Check/install target
    if ! check_target "$target"; then
        continue
    fi

    # Create target-specific output directory
    TARGET_OUTPUT="$OUTPUT_DIR/$target"
    mkdir -p "$TARGET_OUTPUT"

    # Build each plugin
    for plugin in "${PLUGINS[@]}"; do
        echo "  Building $plugin..."

        # Build as staticlib (need to modify Cargo.toml or use RUSTFLAGS)
        RUSTFLAGS="-C target-feature=+crt-static" \
        cargo build -p "$plugin" $BUILD_FLAGS --target "$target" 2>&1 | grep -v "^   Compiling" || true

        # Source path - static lib is in deps directory
        src_dir="$SCRIPT_DIR/target/$target/$PROFILE"
        lib_name=$(get_staticlib_name "$plugin" "$target")

        # Try to find the static lib (might be in deps/)
        src="$src_dir/$lib_name"
        if [ ! -f "$src" ]; then
            src="$src_dir/deps/$lib_name"
        fi
        if [ ! -f "$src" ]; then
            # Try without lib prefix
            src="$src_dir/${plugin}.a"
        fi
        if [ ! -f "$src" ]; then
            src="$src_dir/deps/${plugin}.a"
        fi

        # For rlib (Rust library format)
        rlib="$src_dir/lib${plugin}.rlib"
        if [ ! -f "$src" ] && [ -f "$rlib" ]; then
            src="$rlib"
            lib_name="lib${plugin}.rlib"
        fi

        # Destination
        dst="$TARGET_OUTPUT/$lib_name"

        if [ -f "$src" ]; then
            cp "$src" "$dst"
            size=$(ls -lh "$dst" | awk '{print $5}')
            echo "    -> $lib_name ($size)"
        else
            echo "    Warning: Static lib not found for $plugin"
            echo "    Searched: $src_dir/{,deps/}lib${plugin}.{a,rlib}"
        fi
    done

    # Create archive manifest for this target
    MANIFEST="$TARGET_OUTPUT/libs.json"
    cat > "$MANIFEST" << EOF
{
  "target": "$target",
  "profile": "$PROFILE",
  "libs": [
EOF

    first=true
    for plugin in "${PLUGINS[@]}"; do
        lib_name=$(get_staticlib_name "$plugin" "$target")
        lib_file="$TARGET_OUTPUT/$lib_name"

        # Also check for rlib
        if [ ! -f "$lib_file" ]; then
            lib_name="lib${plugin}.rlib"
            lib_file="$TARGET_OUTPUT/$lib_name"
        fi

        if [ -f "$lib_file" ]; then
            size=$(stat -f%z "$lib_file" 2>/dev/null || stat -c%s "$lib_file" 2>/dev/null || echo "0")
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
      "file": "$lib_name",
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
done

# Generate combined manifest
COMBINED_MANIFEST="$OUTPUT_DIR/manifest.json"
echo "Generating combined manifest..."

cat > "$COMBINED_MANIFEST" << 'EOF'
{
  "type": "static",
  "targets": [
EOF

first_target=true
for target in "${TARGETS[@]}"; do
    TARGET_OUTPUT="$OUTPUT_DIR/$target"
    if [ -d "$TARGET_OUTPUT" ]; then
        if [ "$first_target" = true ]; then
            first_target=false
        else
            echo "," >> "$COMBINED_MANIFEST"
        fi
        echo "    \"$target\"" >> "$COMBINED_MANIFEST"
    fi
done

cat >> "$COMBINED_MANIFEST" << 'EOF'
  ],
  "plugins": [
EOF

first_plugin=true
for plugin in "${PLUGINS[@]}"; do
    version=$(get_plugin_version "$plugin")
    if [ "$first_plugin" = true ]; then
        first_plugin=false
    else
        echo "," >> "$COMBINED_MANIFEST"
    fi
    cat >> "$COMBINED_MANIFEST" << EOF
    {
      "name": "$plugin",
      "version": "$version"
    }
EOF
done

cat >> "$COMBINED_MANIFEST" << 'EOF'
  ]
}
EOF

echo ""
echo "======================================"
echo "Build complete!"
echo "======================================"
echo "Output directory: $OUTPUT_DIR"
echo ""
echo "Directory structure:"
find "$OUTPUT_DIR" -type f -name "*.a" -o -name "*.rlib" -o -name "*.lib" -o -name "*.json" 2>/dev/null | head -30
