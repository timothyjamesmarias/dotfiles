return {
	"ludovicchabant/vim-gutentags",
	config = function()
		vim.g.gutentags_cache_dir = vim.fn.expand("~/.cache/tags")
		vim.g.gutentags_ctags_extra_args = {
			"--languages=Ruby,PHP,HTML,CSS,JavaScript,Sass",
			"--tag-relative=yes",
			"--fields=+l",
		}
		vim.g.gutentags_file_list_command = "rg --files" -- fast file scan
	end,
}
