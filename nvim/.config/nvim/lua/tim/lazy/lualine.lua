return {
	"nvim-lualine/lualine.nvim",
	dependencies = {
		"rmagatti/auto-session",
	},
	config = function()
		local lualine = require("lualine")
		lualine.setup({
			options = {
				icons_enabled = true,
				theme = "auto",
				component_separators = { left = "|", right = "|" },
				section_separators = { left = "", right = "" },
				disabled_filetypes = {
					statusline = {},
					winbar = {},
				},
				ignore_focus = {},
				always_divide_middle = true,
				globalstatus = false,
				refresh = {
					statusline = 1000,
					tabline = 1000,
					winbar = 1000,
				},
			},
			sections = {
				lualine_a = { "mode" },
				lualine_b = { "branch", "diagnostics" },
				lualine_c = {
					"filename",
					{
						"buffers",
						mode = 2,
						{
							mode = 2,
							symbols = {
								modified = " ●",
								alternate_file = "#",
								directory = "",
							},
						},
					},
				},
				lualine_x = { "searchcount", "fileformat", "filetype" },
				lualine_y = { "progress" },
				lualine_z = { "location" },
			},
			inactive_sections = {
				lualine_a = {},
				lualine_b = {},
				lualine_c = { "filename" },
				lualine_x = { "location" },
				lualine_y = {},
				lualine_z = {},
			},
			tabline = {},
			winbar = {},
			inactive_winbar = {},
			extensions = {},
		})
	end,
}
