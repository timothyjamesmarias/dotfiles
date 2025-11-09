return {
	"ludovicchabant/vim-gutentags",
	config = function()
		vim.g.gutentags_enabled = 1
		vim.g.gutentags_cache_dir = vim.fn.expand("~/.cache/tags")
		vim.g.gutentags_generate_on_new = 1
		vim.g.gutentags_generate_on_missing = 1
		vim.g.gutentags_generate_on_write = 1
		vim.g.gutentags_ctags_extra_args = {
			"--options=" .. vim.fn.expand("~") .. "/dotfiles/.ctags",
		}
		vim.g.gutentags_file_list_command = "rg --files"

		-- Project root markers
		vim.g.gutentags_project_root = { ".git", "Gemfile", "package.json" }

		-- Autocommand to generate tags on first open if tags file doesn't exist
		vim.api.nvim_create_autocmd("VimEnter", {
			pattern = "*",
			callback = function()
				local tags_file = vim.fn.findfile("tags", ".;")
				if tags_file == "" then
					vim.fn.jobstart({ "ctags", "-R", "-f", "tags", "--options=" .. vim.fn.expand("~") .. "/dotfiles/.ctags", "." })
				end
			end,
		})
	end,
}
