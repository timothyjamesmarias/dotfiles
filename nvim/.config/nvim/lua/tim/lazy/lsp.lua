return {
	"neovim/nvim-lspconfig",
	lazy = false,
	dependencies = {
		{ "antosha417/nvim-lsp-file-operations", config = true },
		"hrsh7th/nvim-cmp",
		"hrsh7th/cmp-nvim-lsp",
		"onsails/lspkind-nvim",
		"nvim-telescope/telescope.nvim",
		"mason-org/mason.nvim",
		"mason-org/mason-lspconfig.nvim",
	},
	config = function()
		local lspconfig = require("lspconfig")
		local capabilities = require("cmp_nvim_lsp").default_capabilities()
		local lspkind = require("lspkind")

		require("mason").setup()
		require("mason-lspconfig").setup({
			ensure_installed = { "lua_ls", "ruby_lsp", "ts_ls", "html", "cssls" },
			automatic_installation = true,
		})

		local function lsp_setup(server, opts)
			lspconfig[server].setup(vim.tbl_deep_extend("force", {
				on_attach = on_attach,
				capabilities = capabilities,
			}, opts or {}))
		end

		-- LSP servers
		lsp_setup("html", { filetypes = { "html", "eruby", "blade" } })
		lsp_setup("cssls", { filetypes = { "html", "css", "scss" } })
		lsp_setup("ts_ls", { filetypes = { "javascript", "typescript", "html" } })
		lsp_setup("lua_ls", {
			settings = {
				Lua = {
					runtime = { version = "LuaJIT" },
					diagnostics = { globals = { "vim", "require" } },
					workspace = { library = vim.api.nvim_get_runtime_file("", true) },
					telemetry = { enable = false },
				},
			},
		})
		lsp_setup("clangd", { filetypes = { "c", "cpp" } })
		lsp_setup("sqlls", { filetypes = { "sql", "mysql", "pgsql" } })
		lsp_setup("intelephense")
		lsp_setup("rust_analyzer")
		lsp_setup("gopls")
		lsp_setup("ruby_lsp")

		-- Completion config
		local cmp = require("cmp")
		cmp.setup({
			formatting = { format = lspkind.cmp_format() },
			mapping = {
				["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
				["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
				["<CR>"] = cmp.mapping.confirm({ select = true }),
			},
			sources = {
				{ name = "nvim_lsp" },
				{ name = "path" },
			},
			experimental = {
				ghost_text = true,
			},
		})
	end,
}
