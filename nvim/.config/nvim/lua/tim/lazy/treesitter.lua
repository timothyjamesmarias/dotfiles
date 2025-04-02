return {
	"nvim-treesitter/nvim-treesitter",
	event = { "BufReadPre", "BufNewFile" },
	build = "<cmd>TSUpdate",
	dependencies = {
		"windwp/nvim-ts-autotag",
		"JoosepAlviste/nvim-ts-context-commentstring",
		"EmranMR/tree-sitter-blade",
		"RRethy/nvim-treesitter-endwise",
		"vrischmann/tree-sitter-templ",
	},
	config = function()
		local treesitter = require("nvim-treesitter.configs")
		treesitter.setup({
			highlight = { enable = true, additional_vim_regex_highlighting = false },
			modules = {},
			indent = { enable = true },
			autotag = { enable = true },
			ensure_installed = "all",
      ignore_install = { "wing" },
			auto_install = true,
			context_commentstring = {
				enable = true,
			},
			endwise = {
				enable = true,
			},
		})
	end,
}
