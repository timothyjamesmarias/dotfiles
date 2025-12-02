return {
	"nvim-telescope/telescope.nvim",
  branch = "master",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-telescope/telescope-file-browser.nvim",
		{
			"nvim-telescope/telescope-fzf-native.nvim",
			build = "make",
		},
		"nvim-treesitter/nvim-treesitter",
		"nvim-telescope/telescope-ui-select.nvim",
	},
	config = function()
		local telescope = require("telescope")
		local actions = require("telescope.actions")

		telescope.setup({
			defaults = {
				vimgrep_arguments = {
					"rg",
					"--color=never",
					"--no-heading",
					"--with-filename",
					"--line-number",
					"--column",
					"--smart-case",
					"--hidden",
					"--glob=!.git/",
					"--fixed-strings", -- Literal string search (not regex)
				},
				mappings = {
					n = {
						["q"] = actions.close,
					},
					i = {
					},
				},
				layout_config = {
					prompt_position = "top",
					width = 0.9,
					height = 0.6,
				},
				sorting_strategy = "ascending",
				border = true,
				hidden = true,
				file_ignore_patterns = {
					".git/",
				},
			},
			pickers = {
			buffers = {
				mappings = {
					i = {
						["<C-x>"] = actions.delete_buffer,
					},
					n = {
						["<C-x>"] = actions.delete_buffer,
					},
				},
			},
		},
			extensions = {
				fzf = {
					fuzzy = true,
					override_generic_sorter = true,
					override_file_sorter = true,
					case_mode = "smart_case",
				},
				file_browser = {},
				ui_select = {
					require("telescope.themes").get_cursor({
						results_title = false,
						previewer = false,
						border = true,
					}),
				},
			},
		})

		pcall(telescope.load_extension, "fzf")
		pcall(telescope.load_extension, "file_browser")
		pcall(telescope.load_extension, "ui-select")
	end,
}
