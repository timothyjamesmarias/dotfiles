return {
	"sainnhe/everforest",
	priority = 1000,

	config = function()
		vim.g.everforest_enable_italic = true
		vim.g.everforest_background = "hard"
    vim.g.everforest_show_eob = true
		vim.cmd.colorscheme("everforest")
	end,
}
