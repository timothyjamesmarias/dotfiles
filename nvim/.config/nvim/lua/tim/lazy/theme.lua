return {
	"rose-pine/neovim",
	dependencies = {
		"rktjmp/lush.nvim",
	},
	priority = 1000,
	config = function()
		vim.cmd([[colorscheme rose-pine]])
	end,
}
