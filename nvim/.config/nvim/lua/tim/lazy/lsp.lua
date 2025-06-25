return {
	"neovim/nvim-lspconfig",
	lazy = false,
	dependencies = {
		{ "antosha417/nvim-lsp-file-operations", config = true },
		"hrsh7th/nvim-cmp",
		"hrsh7th/cmp-nvim-lsp",
		"hrsh7th/cmp-buffer",
		"hrsh7th/cmp-path",
		"hrsh7th/cmp-nvim-lua",
		"hrsh7th/cmp-nvim-lsp-signature-help",
		"onsails/lspkind-nvim",
		"nvim-telescope/telescope.nvim",
	},
	config = function()
		local lspconfig = require("lspconfig")
		local capabilities = require("cmp_nvim_lsp").default_capabilities()
		local cmp = require("cmp")
		local lspkind = require("lspkind")

		local on_attach = function()
			vim.keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action)
			vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename)
			vim.keymap.set("n", "K", vim.lsp.buf.hover)
			vim.keymap.set("n", "<leader>rs", "<cmd>LspRestart<CR>")
		end

		lspconfig["html"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
			filetypes = { "html", "eruby", "blade", "templ" },
		})
		lspconfig["sqlls"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
			filetypes = { "sql", "mysql", "pgsql" },
		})
		lspconfig["clangd"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
			filetypes = { "c", "cpp" },
		})
		lspconfig["lua_ls"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
			settings = {
				Lua = {
					runtime = {
						version = "LuaJIT",
					},
					diagnostics = {
						globals = {
							"vim",
							"require",
						},
					},
					workspace = {
						library = vim.api.nvim_get_runtime_file("", true),
					},
					telemetry = {
						enable = false,
					},
				},
			},
		})
		lspconfig["volar"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
			filetypes = { "vue" },
		})
		lspconfig["ts_ls"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
			filetypes = {
				"javascript",
				"typescript",
				"javascriptreact",
				"typescriptreact",
				"html",
			},
		})
		lspconfig["tailwindcss"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
			filetypes = {
				"html",
				"eruby",
				"blade",
				"vue",
				"javascript",
				"typescript",
				"javascriptreact",
				"typescriptreact",
				"templ",
			},
			init_options = { userLanguages = { templ = "html" } },
		})
		lspconfig["cssls"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
			filetypes = {
				"html",
				"css",
			},
		})
		lspconfig["intelephense"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		lspconfig["rust_analyzer"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		lspconfig["gopls"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		lspconfig["ruby_lsp"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		cmp.setup({
			formatting = {
				format = lspkind.cmp_format(),
			},
			mapping = {
				["<C-n>"] = cmp.mapping(function(fallback)
					if cmp.visible() then
						cmp.select_next_item()
					else
						fallback()
					end
				end, { "i", "s" }),
				["<C-p>"] = cmp.mapping(function(fallback)
					if cmp.visible() then
						cmp.select_prev_item()
					else
						fallback()
					end
				end, { "i", "s" }),
				["<CR>"] = cmp.mapping.confirm({ select = true }),
			},
			sources = {
				{ name = "nvim_lsp" },
				{ name = "nvim_lsp_signature_help" },
				{ name = "buffer" },
				{ name = "path" },
			},
			experimental = {
				native_menu = false,
				ghost_text = true,
			},
		})
	end,
}
