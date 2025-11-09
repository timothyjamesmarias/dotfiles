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

			-- Kotlin-specific keymaps
			vim.api.nvim_create_autocmd("FileType", {
				pattern = { "kotlin", "java" },
				callback = function()
					local opts = { buffer = true, silent = true, noremap = true }

					-- Gradle integration keymaps
					vim.keymap.set("n", "<leader>gb", ":!./gradlew build<CR>", opts)
					vim.keymap.set("n", "<leader>gt", ":!./gradlew test<CR>", opts)
					vim.keymap.set("n", "<leader>gr", ":!./gradlew run<CR>", opts)
					vim.keymap.set("n", "<leader>gc", ":!./gradlew clean<CR>", opts)
					vim.keymap.set("n", "<leader>gd", ":!./gradlew bootRun<CR>", opts)

					-- Quick compile check (no tests)
					vim.keymap.set("n", "<leader>gq", ":!./gradlew build -x test<CR>", opts)

					-- Run specific test under cursor (requires custom function)
					vim.keymap.set("n", "<leader>gT", function()
						local class_name = vim.fn.expand("%:t:r")
						vim.cmd("!./gradlew test --tests " .. class_name)
					end, opts)
				end,
			})
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
