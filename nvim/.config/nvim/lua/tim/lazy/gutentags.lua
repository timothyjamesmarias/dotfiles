return {
	"ludovicchabant/vim-gutentags",
	config = function()
		vim.g.gutentags_enabled = 1
		vim.g.gutentags_cache_dir = vim.fn.expand("~/.cache/tags")
		vim.g.gutentags_generate_on_new = 1
		vim.g.gutentags_generate_on_missing = 1
		vim.g.gutentags_generate_on_write = 1
		vim.g.gutentags_file_list_command = "rg --files"

		-- Project root markers
		vim.g.gutentags_project_root = { ".git", "Gemfile", "package.json", "composer.json" }

		-- Exclude these directories from tag generation
		vim.g.gutentags_ctags_exclude = {
			"node_modules",
			"vendor",
			".git",
			"dist",
			"build",
			"coverage",
		}
	end,
}
