#!/bin/bash
# Create a macOS .app bundle for Lem

set -e

LEM_BINARY="$HOME/opt/lem/lem"
APP_NAME="Lem"
APP_DIR="$HOME/Applications/${APP_NAME}.app"

echo "Creating macOS application bundle for Lem..."

# Check if Lem binary exists
if [ ! -f "$LEM_BINARY" ]; then
    echo "Error: Lem binary not found at $LEM_BINARY"
    echo "Please install Lem first using install-lem.sh"
    exit 1
fi

# Create application bundle structure
echo "Creating app bundle at $APP_DIR"
mkdir -p "$APP_DIR/Contents/MacOS"
mkdir -p "$APP_DIR/Contents/Resources"

# Create launcher script
cat > "$APP_DIR/Contents/MacOS/Lem" << 'LAUNCHER_EOF'
#!/bin/bash
# Lem launcher script

# Set up environment
export PATH="$HOME/.qlot/bin:$HOME/.local/bin:/opt/homebrew/bin:$PATH"

# Get the directory of the Lem binary
LEM_DIR="$HOME/opt/lem"

# Launch Lem
exec "$LEM_DIR/lem" "$@"
LAUNCHER_EOF

chmod +x "$APP_DIR/Contents/MacOS/Lem"

# Create Info.plist
cat > "$APP_DIR/Contents/Info.plist" << 'PLIST_EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleExecutable</key>
    <string>Lem</string>
    <key>CFBundleIdentifier</key>
    <string>com.lem-project.lem</string>
    <key>CFBundleName</key>
    <string>Lem</string>
    <key>CFBundleDisplayName</key>
    <string>Lem Editor</string>
    <key>CFBundleVersion</key>
    <string>1.0</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleSignature</key>
    <string>????</string>
    <key>LSMinimumSystemVersion</key>
    <string>10.15</string>
    <key>NSHighResolutionCapable</key>
    <true/>
    <key>CFBundleIconFile</key>
    <string>AppIcon.icns</string>
    <key>CFBundleDocumentTypes</key>
    <array>
        <dict>
            <key>CFBundleTypeExtensions</key>
            <array>
                <string>*</string>
            </array>
            <key>CFBundleTypeName</key>
            <string>Text File</string>
            <key>CFBundleTypeRole</key>
            <string>Editor</string>
            <key>LSHandlerRank</key>
            <string>Alternate</string>
        </dict>
    </array>
</dict>
</plist>
PLIST_EOF

# Copy the Lem icon if it exists
ICON_FILE="$(dirname "${BASH_SOURCE[0]}")/Lem.icns"
if [ -f "$ICON_FILE" ]; then
    echo "Installing Lem icon..."
    cp "$ICON_FILE" "$APP_DIR/Contents/Resources/AppIcon.icns"
else
    echo "Warning: Lem.icns not found at $ICON_FILE"
    echo "Run create-lem-icon.sh to generate the icon from lem.svg"
    # Create placeholder
    touch "$APP_DIR/Contents/Resources/AppIcon.icns"
fi

echo "✅ Application bundle created successfully!"
echo
echo "Location: $APP_DIR"
echo
echo "You can now:"
echo "  1. Open Spotlight (Cmd+Space) and type 'Lem'"
echo "  2. Drag Lem.app to your Dock"
echo "  3. Double-click Lem.app in ~/Applications"
echo
echo "Note: On first launch, you may need to:"
echo "  - Right-click the app and select 'Open' (to bypass Gatekeeper)"
echo "  - Or run: xattr -cr '$APP_DIR'"
echo

# Ask if user wants to remove quarantine attribute
read -p "Remove quarantine attribute now? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    xattr -cr "$APP_DIR"
    echo "✅ Quarantine attribute removed. App should launch without warnings."
fi

echo
echo "Installation complete!"