function vim.getVisualSelection()
	local _, ls_row, ls_col, _ = unpack(vim.fn.getpos("v"))
	local _, le_row, le_col, _ = unpack(vim.fn.getpos("."))
	if ls_row > le_row or (ls_row == le_row and ls_col > le_col) then
		ls_row, le_row = le_row, ls_row
		ls_col, le_col = le_col, ls_col
	end
	local lines = vim.fn.getline(ls_row, le_row)
	if #lines == 0 then
		return ""
	end
	lines[1] = string.sub(lines[1], ls_col)
	lines[#lines] = string.sub(lines[#lines], 1, le_col)
	return table.concat(lines, " ")
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
		"nvim-telescope/telescope-ui-select.nvim",
	},
	config = function()
		local telescope = require("telescope")
		telescope.setup({
			defaults = {
				mappings = {
					n = {
						["q"] = require("telescope.actions").close,
					},
					i = {
						["<C-l>"] = require("telescope.actions").close,
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
			pickers = {},
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

		local builtin = require("telescope.builtin")
		vim.keymap.set("n", "<leader>ff", "<cmd>Telescope find_files follow=true hidden=true<CR>", { silent = true })
		vim.keymap.set("v", "<leader>fw", function()
			local text = vim.getVisualSelection()
			require("telescope.builtin").live_grep({ default_text = text })
		end)
		vim.keymap.set("v", "<leader>ff", function()
			local text = vim.getVisualSelection()
			builtin.find_files({ default_text = text })
		end, { silent = true, noremap = true })
		vim.keymap.set("n", "<leader>fr", builtin.oldfiles, { silent = true })
		vim.keymap.set("n", "<leader>fg", builtin.git_commits, { silent = true })
		vim.keymap.set("n", "<leader>fw", builtin.live_grep, { silent = true })
		vim.keymap.set("n", "<leader>fk", builtin.keymaps, { silent = true })
		vim.keymap.set("n", "<leader>fcc", builtin.commands, { silent = true })
		vim.keymap.set("n", "<leader>fca", builtin.autocommands, { silent = true })
		vim.keymap.set("n", "<leader>fj", builtin.jumplist, { silent = true })
		vim.keymap.set("n", "<leader>fb", builtin.buffers, { silent = true })
		vim.keymap.set("n", "<leader>fh", builtin.help_tags, { silent = true })
		vim.keymap.set("n", "<leader>sp", builtin.spell_suggest, { silent = true })
		vim.keymap.set("n", "<leader>fn", function()
			require("telescope").extensions.file_browser.file_browser({
				path = "%:p:h",
				select_buffer = true,
				grouped = true,
				hidden = true,
				previewer = false,
				layout_config = { height = 0.5 },
			})
		end, { noremap = true, silent = true })
		vim.keymap.set("n", "<leader>fp", function()
			local git_root = vim.fn.systemlist("git rev-parse --show-toplevel")[1]
			require("telescope").extensions.file_browser.file_browser({
				path = git_root,
				grouped = true,
				hidden = true,
				previewer = false,
				layout_config = { height = 0.5 },
			})
		end, { noremap = true, silent = true })
		vim.keymap.set("n", "<leader>tg", builtin.tagstack, { silent = true })
		vim.keymap.set("n", "<leader>tt", builtin.tags, { silent = true })
		vim.keymap.set("n", "<leader>sl", builtin.grep_string, { silent = true, noremap = true })
		vim.keymap.set("v", "<leader>sl", function()
			local text = vim.getVisualSelection()
			builtin.grep_string({ search = text })
		end, { silent = true, noremap = true })
	end,
}
