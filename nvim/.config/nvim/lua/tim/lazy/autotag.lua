return {
	"windwp/nvim-ts-autotag",
	dependencies = "nvim-treesitter/nvim-treesitter",
	config = function()
		require("nvim-ts-autotag").setup({
			autotag = {
				filetypes = {
					"html",
					"xml",
					"embedded_template",
					"eruby",
					"php",
					"js",
					"ts",
					"jsx",
					"tsx",
					"vue",
				},
			},
		})
	end,
	lazy = true,
	event = "VeryLazy",
}
