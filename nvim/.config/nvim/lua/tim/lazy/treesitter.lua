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
		-- Crystal parser setup for nvim-treesitter v0.10+
		local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
		parser_config.crystal = {
			install_info = {
				url = "https://github.com/crystal-lang-tools/tree-sitter-crystal",
				files = { "src/parser.c", "src/scanner.c" },
				branch = "main",
				generate_requires_npm = false,
				requires_generate_from_grammar = false,
			},
			filetype = "crystal",
		}

		vim.treesitter.language.register("crystal", "cr")

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
