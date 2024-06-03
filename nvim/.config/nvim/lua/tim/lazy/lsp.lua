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
		"saadparwaiz1/cmp_luasnip",
		"L3MON4D3/LuaSnip",
		"onsails/lspkind-nvim",
		"nvim-telescope/telescope.nvim",
		"rafamadriz/friendly-snippets",
	},
	config = function()
		local lspconfig = require("lspconfig")
		local capabilities = require("cmp_nvim_lsp").default_capabilities()
		local cmp = require("cmp")
		local lspkind = require("lspkind")
		local luasnip = require("luasnip")

		local on_attach = function()
			vim.keymap.set("n", "gr", "<cmd>Telescope lsp_references<CR>")
			vim.keymap.set("n", "gD", vim.lsp.buf.declaration)
			vim.keymap.set("n", "gd", "<cmd>Telescope lsp_definitions<CR>")
			vim.keymap.set("n", "gi", "<cmd>Telescope lsp_implementations<CR>")
			vim.keymap.set("n", "gt", "<cmd>Telescope lsp_type_definitions<CR>")
			vim.keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action)
			vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename)
			vim.keymap.set("n", "<leader>en", vim.diagnostic.goto_prev)
			vim.keymap.set("n", "<leader>ep", vim.diagnostic.goto_next)
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
		lspconfig["tsserver"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
			filetypes = {
				"javascript",
				"typescript",
				"javascriptreact",
				"typescriptreact",
				"html",
				"eruby",
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
		lspconfig["bashls"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		lspconfig["dockerls"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		lspconfig["marksman"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		lspconfig["intelephense"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		lspconfig["solargraph"].setup({
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
		lspconfig["templ"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		lspconfig["zls"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		lspconfig["pyright"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})
		lspconfig["ocamllsp"].setup({
			on_attach = on_attach,
			capabilities = capabilities,
		})

		cmp.setup({
			snippet = {
				expand = function(args)
					require("luasnip").lsp_expand(args.body)
				end,
			},
			formatting = {
				format = lspkind.cmp_format(),
			},
			mapping = {
				["<C-n>"] = cmp.mapping(function(fallback)
					if cmp.visible() then
						cmp.select_next_item()
					elseif luasnip.expand_or_jumpable() then
						luasnip.expand_or_jump()
					else
						fallback()
					end
				end, { "i", "s" }),
				["<C-p>"] = cmp.mapping(function(fallback)
					if cmp.visible() then
						cmp.select_prev_item()
					elseif luasnip.jumpable(-1) then
						luasnip.jump(-1)
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
				{ name = "luasnip" },
			},
			experimental = {
				native_menu = false,
				ghost_text = true,
			},
		})
	end,
}
