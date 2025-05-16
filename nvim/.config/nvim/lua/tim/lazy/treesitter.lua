return {
	"nvim-treesitter/nvim-treesitter",
	event = { "BufReadPre", "BufNewFile" },
	build = ":TSUpdate",
	dependencies = {
		"windwp/nvim-ts-autotag",
		"JoosepAlviste/nvim-ts-context-commentstring",
		"EmranMR/tree-sitter-blade",
		"RRethy/nvim-treesitter-endwise",
	},
	config = function()
		local treesitter = require("nvim-treesitter.configs")
		treesitter.setup({
			highlight = { enable = true, additional_vim_regex_highlighting = false },
			modules = {},
			indent = { enable = true },
			ensure_installed = "all",
			ignore_install = { "wing" },
			auto_install = true,
			endwise = {
				enable = true,
			},
		})

    require("nvim-ts-autotag").setup()
    require("ts_context_commentstring").setup({})
	end,
}
