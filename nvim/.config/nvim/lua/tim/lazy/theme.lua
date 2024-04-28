return {
	"folke/tokyonight.nvim",
	dependencies = {
		"rktjmp/lush.nvim",
	},
	priority = 1000,
	config = function()
		vim.cmd([[colorscheme tokyonight-night]])
	end,
}
