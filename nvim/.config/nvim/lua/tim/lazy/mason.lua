return {
	"williamboman/mason.nvim",
	dependencies = {
		"williamboman/mason-lspconfig.nvim",
	},
	config = function()
		local mason = require("mason")
		local mason_lspconfig = require("mason-lspconfig")
		mason.setup()
		mason_lspconfig.setup()
		vim.keymap.set("n", "<leader>M", "<cmd>Mason<CR>")
	end,
}
