return {
	"navarasu/onedark.nvim",
	dependencies = {
		"rktjmp/lush.nvim",
	},
	priority = 1000,
	config = function()
		local onedark = require("onedark").setup({
			style = "deep",
		})
		vim.cmd([[colorscheme onedark]])
	end,
}
