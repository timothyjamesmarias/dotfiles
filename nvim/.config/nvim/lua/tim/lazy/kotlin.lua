return {
	-- Kotlin syntax highlighting and support
	{
		"udalov/kotlin-vim",
		ft = { "kotlin" },
	},

	-- Enhanced LSP configuration for Kotlin/Java projects
	{
		"mfussenegger/nvim-jdtls",
		ft = { "java", "kotlin" },
		dependencies = {
			"neovim/nvim-lspconfig",
		},
		config = function()
			-- This will be configured per-project using .nvim.lua or via autocommands
			-- See the java.lua example in the scripts directory for project-specific setup
		end,
	},

	-- Gradle syntax support
	{
		"vim-scripts/groovy.vim",
		ft = { "groovy", "gradle" },
	},

	-- Better Gradle Kotlin DSL support
	{
		"neovim/nvim-lspconfig",
		opts = function()
			return {
				servers = {
					-- Enhanced Kotlin LSP settings
					kotlin_language_server = {
						settings = {
							kotlin = {
								compiler = {
									jvm = {
										target = "17", -- Adjust based on your project
									},
								},
								linting = {
									enabled = true,
								},
								completion = {
									snippets = {
										enabled = true,
									},
								},
							},
						},
					},
				},
			}
		end,
	},
}
