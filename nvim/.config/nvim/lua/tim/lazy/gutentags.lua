return {
	"ludovicchabant/vim-gutentags",
	config = function()
		vim.g.gutentags_enabled = 1
		vim.g.gutentags_cache_dir = vim.fn.expand("~/.cache/tags")
		vim.g.gutentags_generate_on_new = 1
		vim.g.gutentags_generate_on_missing = 1
		vim.g.gutentags_generate_on_write = 1

		-- Use ripgrep for fast file listing
		vim.g.gutentags_file_list_command = "rg --files"

		-- Use Universal Ctags executable
		vim.g.gutentags_ctags_executable = "ctags"

		-- Extra arguments for better tag generation
		vim.g.gutentags_ctags_extra_args = {
			"--fields=+ailmnS",
			"--extras=+q",
			"--pattern-length-limit=250",
		}

		-- Project root markers (order matters - first match wins)
		vim.g.gutentags_project_root = {
			".git",
			"Gemfile",
			"package.json",
			"composer.json",
			"go.mod",
			"Cargo.toml",
		}

		-- Exclude these directories from tag generation
		vim.g.gutentags_ctags_exclude = {
			"node_modules",
			"vendor",
			".git",
			"dist",
			"build",
			"coverage",
			"*.min.js",
			"*.min.css",
			"*.map",
			"public/packs",
			"public/assets",
		}

		-- Show progress in status line (optional)
		vim.g.gutentags_generate_on_empty_buffer = 0

		-- Debugging (set to 1 to see gutentags activity)
		vim.g.gutentags_trace = 0
	end,
}
