#!/bin/bash
# Create macOS .icns icon from Lem SVG logo

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SVG_FILE="$SCRIPT_DIR/lem.svg"
ICONSET_DIR="$SCRIPT_DIR/Lem.iconset"
ICNS_FILE="$SCRIPT_DIR/Lem.icns"
APP_DIR="$HOME/Applications/Lem.app"

echo "Creating macOS icon from Lem logo..."

# Check if SVG exists
if [ ! -f "$SVG_FILE" ]; then
    echo "Error: lem.svg not found at $SVG_FILE"
    exit 1
fi

# Create iconset directory
rm -rf "$ICONSET_DIR"
mkdir -p "$ICONSET_DIR"

# Generate PNG files at required sizes
# macOS requires specific naming: icon_SIZExSIZE[@2x].png
echo "Generating PNG files..."

# 16x16
rsvg-convert -w 16 -h 16 "$SVG_FILE" -o "$ICONSET_DIR/icon_16x16.png"
rsvg-convert -w 32 -h 32 "$SVG_FILE" -o "$ICONSET_DIR/icon_16x16@2x.png"

# 32x32
rsvg-convert -w 32 -h 32 "$SVG_FILE" -o "$ICONSET_DIR/icon_32x32.png"
rsvg-convert -w 64 -h 64 "$SVG_FILE" -o "$ICONSET_DIR/icon_32x32@2x.png"

# 128x128
rsvg-convert -w 128 -h 128 "$SVG_FILE" -o "$ICONSET_DIR/icon_128x128.png"
rsvg-convert -w 256 -h 256 "$SVG_FILE" -o "$ICONSET_DIR/icon_128x128@2x.png"

# 256x256
rsvg-convert -w 256 -h 256 "$SVG_FILE" -o "$ICONSET_DIR/icon_256x256.png"
rsvg-convert -w 512 -h 512 "$SVG_FILE" -o "$ICONSET_DIR/icon_256x256@2x.png"

# 512x512
rsvg-convert -w 512 -h 512 "$SVG_FILE" -o "$ICONSET_DIR/icon_512x512.png"
rsvg-convert -w 1024 -h 1024 "$SVG_FILE" -o "$ICONSET_DIR/icon_512x512@2x.png"

echo "Converting to .icns format..."
iconutil -c icns "$ICONSET_DIR" -o "$ICNS_FILE"

echo "Cleaning up temporary files..."
rm -rf "$ICONSET_DIR"

echo "✅ Icon created: $ICNS_FILE"

# Update the app bundle if it exists
if [ -d "$APP_DIR" ]; then
    echo
    echo "Updating Lem.app icon..."
    cp "$ICNS_FILE" "$APP_DIR/Contents/Resources/AppIcon.icns"

    # Update Info.plist to reference the icon
    if [ -f "$APP_DIR/Contents/Info.plist" ]; then
        # Add CFBundleIconFile key if not present
        /usr/libexec/PlistBuddy -c "Delete :CFBundleIconFile" "$APP_DIR/Contents/Info.plist" 2>/dev/null || true
        /usr/libexec/PlistBuddy -c "Add :CFBundleIconFile string AppIcon.icns" "$APP_DIR/Contents/Info.plist"
    fi

    # Clear icon cache to force macOS to update
    touch "$APP_DIR"
    killall Finder 2>/dev/null || true
    killall Dock 2>/dev/null || true

    echo "✅ App bundle icon updated!"
    echo
    echo "The new icon should appear in:"
    echo "  - Finder"
    echo "  - Dock (if pinned)"
    echo "  - Spotlight search"
    echo
    echo "Note: It may take a few seconds for the icon to update everywhere."
else
    echo
    echo "App bundle not found at $APP_DIR"
    echo "Run create-lem-app.sh first, or copy the icon manually:"
    echo "  cp $ICNS_FILE \$APP_DIR/Contents/Resources/AppIcon.icns"
fi

echo
echo "Done!"
