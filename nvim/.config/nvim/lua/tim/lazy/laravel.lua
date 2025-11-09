return {
	"adalessa/laravel.nvim",
	dependencies = {
		"nvim-telescope/telescope.nvim",
		"tpope/vim-dotenv",
		"MunifTanjim/nui.nvim",
		"nvimtools/none-ls.nvim",
		"nvim-neotest/nvim-nio",
	},
	cmd = { "Sail", "Artisan", "Composer", "Npm", "Yarn", "Laravel" },
	ft = { "php", "blade" },
	keys = {
		{ "<leader>pa", ":Laravel artisan<cr>", desc = "Project: Artisan" },
		{ "<leader>pr", ":Laravel routes<cr>", desc = "Project: Routes" },
		{ "<leader>pm", ":Laravel related<cr>", desc = "Project: Related/Model" },
	},
	event = { "VeryLazy" },
	config = function()
		require("laravel").setup()
	end,
}
