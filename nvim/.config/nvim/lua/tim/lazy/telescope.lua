function vim.getVisualSelection()
	vim.cmd('noau normal! "vy"')
	local text = vim.fn.getreg("v")
	vim.fn.setreg("v", {})

	text = string.gsub(text, "\n", "")
	if #text > 0 then
		return text
	else
		return ""
	end
end

return {
	"nvim-telescope/telescope.nvim",
	tag = "0.1.3",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-telescope/telescope-file-browser.nvim",
		{
			"nvim-telescope/telescope-fzf-native.nvim",
			build = "make",
		},
		"nvim-treesitter/nvim-treesitter",
		"stevearc/aerial.nvim",
		"debugloop/telescope-undo.nvim",
		"nvim-telescope/telescope-ui-select.nvim",
	},
	config = function()
		local telescope = require("telescope")
		telescope.setup({
			defaults = {
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
			pickers = {},
			extensions = {
				fzf = {
					fuzzy = true,
					override_generic_sorter = true,
					override_file_sorter = true,
					case_mode = "smart_case",
				},
				aerial = {},
				undo = {},
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
		telescope.load_extension("fzf")
		telescope.load_extension("file_browser")
		telescope.load_extension("aerial")
		telescope.load_extension("undo")
		telescope.load_extension("ui-select")

		local builtin = require("telescope.builtin")
		vim.keymap.set("n", "<leader>ff", "<cmd>Telescope find_files follow=true hidden=true<CR>", { silent = true })
		vim.keymap.set(
			"v",
			"<leader>fw",
      function ()
        local text = vim.getVisualSelection()
        builtin.live_grep({default_text = text})
      end,
			{ silent = true, noremap = true }
		)
		vim.keymap.set(
			"v",
			"<leader>ff",
      function ()
        local text = vim.getVisualSelection()
        builtin.find_files({default_text = text})
      end,
			{ silent = true, noremap = true }
		)
		vim.keymap.set("n", "<leader>fr", builtin.oldfiles, { silent = true })
		vim.keymap.set("n", "<leader>fg", builtin.git_commits, { silent = true })
		vim.keymap.set("n", "<leader>fw", builtin.live_grep, { silent = true })
		vim.keymap.set("n", "<leader>fk", builtin.keymaps, { silent = true })
		vim.keymap.set("n", "<leader>fp", builtin.pickers, { silent = true })
		vim.keymap.set("n", "<leader>fcc", builtin.commands, { silent = true })
		vim.keymap.set("n", "<leader>fca", builtin.autocommands, { silent = true })
		vim.keymap.set("n", "<leader>fb", builtin.buffers, { silent = true })
		vim.keymap.set("n", "<leader>fh", builtin.help_tags, { silent = true })
		vim.keymap.set("n", "<leader>sp", builtin.spell_suggest, { silent = true })
		vim.keymap.set("n", "<leader>fn", "<cmd>Telescope file_browser<CR>", { silent = true, noremap = true })
		vim.keymap.set("n", "<leader>fa", "<cmd>Telescope aerial<CR>", { silent = true, noremap = true })
		vim.keymap.set("n", "<leader>tg", builtin.tagstack, { silent = true })
		vim.keymap.set("n", "<leader>sl", builtin.grep_string, { silent = true, noremap = true })
		vim.keymap.set(
			"v",
			"<leader>sl",
			"y<ESC><cmd>Telescope grep_string default_text=<c-r>0<CR>",
			{ silent = true, noremap = true }
		)
		vim.keymap.set("n", "<leader>u", "<cmd>Telescope undo<CR>")
	end,
}
