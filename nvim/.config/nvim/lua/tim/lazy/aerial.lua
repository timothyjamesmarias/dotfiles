return {
		"stevearc/aerial.nvim",
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			local aerial = require("aerial")
			aerial.setup()
		end,
	}
