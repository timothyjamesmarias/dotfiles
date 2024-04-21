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
			auto_install = true,
			context_commentstring = {
				enable = true,
			},
			endwise = {
				enable = true,
			},
		})
		local treesitter_parser_config = require("nvim-treesitter.parsers").get_parser_configs()
		treesitter_parser_config.templ = {
			install_info = {
				url = "https://github.com/vrischmann/tree-sitter-templ.git",
				files = { "src/parser.c", "src/scanner.c" },
				branch = "master",
			},
		}
		treesitter_parser_config.blade = {
			install_info = {
				url = "https://github.com/EmranMR/tree-sitter-blade",
				files = { "src/parser.c" },
				branch = "main",
			},
			filetype = "blade",
		}
		vim.treesitter.language.register("templ", "templ")
		vim.filetype.add({
			extension = {
				templ = "templ",
			},
		})
		vim.filetype.add({
			pattern = {
				[".*%.blade%.php"] = "blade",
			},
		})
	end,
}
