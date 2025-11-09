#!/usr/bin/env bash
# Setup script for Java/Kotlin projects with JDTLS

set -e

PROJECT_DIR="${1:-.}"
cd "$PROJECT_DIR"

echo "Setting up Java/Kotlin project at: $PROJECT_DIR"

# Create .nvim.lua for project-specific LSP configuration
if [ ! -f ".nvim.lua" ]; then
	cat > .nvim.lua << 'EOF'
-- Project-specific Neovim configuration for Java/Kotlin
local jdtls = require('jdtls')

-- Determine project root
local root_markers = {'gradlew', 'mvnw', '.git', 'pom.xml', 'build.gradle', 'build.gradle.kts'}
local root_dir = require('jdtls.setup').find_root(root_markers)

-- JDTLS installation path (adjust based on your setup)
local jdtls_install = vim.fn.stdpath('data') .. '/mason/packages/jdtls'
local lombok_jar = jdtls_install .. '/lombok.jar'

-- Workspace directory (separate for each project)
local project_name = vim.fn.fnamemodify(root_dir, ':p:h:t')
local workspace_dir = vim.fn.stdpath('data') .. '/jdtls-workspace/' .. project_name

-- JDTLS configuration
local config = {
	cmd = {
		'java',
		'-Declipse.application=org.eclipse.jdt.ls.core.id1',
		'-Dosgi.bundles.defaultStartLevel=4',
		'-Declipse.product=org.eclipse.jdt.ls.core.product',
		'-Dlog.protocol=true',
		'-Dlog.level=ALL',
		'-javaagent:' .. lombok_jar,
		'-Xms1g',
		'--add-modules=ALL-SYSTEM',
		'--add-opens', 'java.base/java.util=ALL-UNNAMED',
		'--add-opens', 'java.base/java.lang=ALL-UNNAMED',
		'-jar', vim.fn.glob(jdtls_install .. '/plugins/org.eclipse.equinox.launcher_*.jar'),
		'-configuration', jdtls_install .. '/config_' .. (vim.fn.has('mac') == 1 and 'mac' or 'linux'),
		'-data', workspace_dir,
	},
	root_dir = root_dir,
	settings = {
		java = {
			eclipse = { downloadSources = true },
			configuration = { updateBuildConfiguration = "interactive" },
			maven = { downloadSources = true },
			implementationsCodeLens = { enabled = true },
			referencesCodeLens = { enabled = true },
			references = { includeDecompiledSources = true },
			format = { enabled = true },
		},
		signatureHelp = { enabled = true },
		completion = {
			favoriteStaticMembers = {
				"org.hamcrest.MatcherAssert.assertThat",
				"org.hamcrest.Matchers.*",
				"org.hamcrest.CoreMatchers.*",
				"org.junit.jupiter.api.Assertions.*",
				"java.util.Objects.requireNonNull",
				"java.util.Objects.requireNonNullElse",
				"org.mockito.Mockito.*",
			},
		},
		sources = {
			organizeImports = {
				starThreshold = 9999,
				staticStarThreshold = 9999,
			},
		},
	},
	init_options = {
		bundles = {},
	},
}

-- Start or attach JDTLS
jdtls.start_or_attach(config)
EOF
	echo "Created .nvim.lua for project-specific LSP setup"
else
	echo ".nvim.lua already exists, skipping..."
fi

# Create gradle.properties if it doesn't exist
if [ ! -f "gradle.properties" ] && [ -f "gradlew" ]; then
	cat > gradle.properties << 'EOF'
# Gradle performance settings
org.gradle.daemon=true
org.gradle.parallel=true
org.gradle.caching=true
org.gradle.configureondemand=true
org.gradle.jvmargs=-Xmx4g -XX:MaxMetaspaceSize=1g -XX:+HeapDumpOnOutOfMemoryError

# Kotlin compiler settings
kotlin.incremental=true
kotlin.incremental.js=true
kotlin.caching.enabled=true
kotlin.daemon.jvmargs=-Xmx2g

# Kotlin compiler warnings
kotlin.compiler.suppressWarnings=false
EOF
	echo "Created gradle.properties with performance optimizations"
else
	echo "gradle.properties already exists or not a Gradle project, skipping..."
fi

echo "Setup complete! Open nvim in this directory to use JDTLS."
