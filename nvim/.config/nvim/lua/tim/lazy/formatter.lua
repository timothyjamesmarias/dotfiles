return {
	"mhartington/formatter.nvim",
	config = function()
		local formatter = require("formatter")
		local util = require("formatter.util")
		formatter.setup({
			filetype = {
				lua = {
					require("formatter.filetypes.lua").stylua,
					function()
						return {
							exe = "stylua",
							args = {
								"--search-parent-directories",
								"--stdin-filepath",
								util.escape_path(util.get_current_buffer_file_path()),
								"--",
								"-",
							},
							stdin = true,
						}
					end,
				},
				rust = {
					exe = "rustfmt",
					args = {
						"--check",
					},
				},
				python = {
					exe = "black",
					args = {
						"--quiet",
						"-",
					},
					stdin = true,
				},
				ocaml = {
					exe = "ocamlformat",
				},
				php = {
					exe = "pint",
				},
				go = {
					require("formatter.filetypes.go").gofmt,
					exe = "gofmt",
					-- args = {
					-- 	"-w",
					-- 	"-s",
					-- },
				},
				["*"] = {
					require("formatter.filetypes.any").remove_trailing_whitespace,
				},
			},
		})
		vim.keymap.set("n", "<leader>fm", "<cmd>Format<CR>", { silent = false })
	end,
}
