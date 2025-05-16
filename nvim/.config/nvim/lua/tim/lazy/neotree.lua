return {
	"nvim-neo-tree/neo-tree.nvim",
	branch = "v3.x",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-tree/nvim-web-devicons",
		"MunifTanjim/nui.nvim",
	},
	config = function()
		require("neo-tree").setup({
			window = {
				width = 50,
			},
			filesystem = {
				filtered_items = {
					visible = true,
					hide_dotfiles = false,
					hide_gitignored = true,
				},
				follow_current_file = {
					enabled = true,
				},
			},
		})
		vim.keymap.set("n", "<leader>nn", ":Neotree toggle<CR>", { silent = true, desc = "Toggle Neo-tree" })
		vim.keymap.set(
			"n",
			"<leader>ns",
			":Neotree reveal<CR>",
			{ silent = true, desc = "Toggle Neo-tree in current directory" }
		)
	end,
}
