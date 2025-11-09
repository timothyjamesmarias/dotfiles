return {
	"neovim/nvim-lspconfig",
	lazy = false,
	dependencies = {
		{ "antosha417/nvim-lsp-file-operations", config = true },
		"hrsh7th/nvim-cmp",
		"hrsh7th/cmp-nvim-lsp",
		"onsails/lspkind-nvim",
		"nvim-telescope/telescope.nvim",
	},
	config = function()
		local lspconfig = require("lspconfig")
		local capabilities = require("cmp_nvim_lsp").default_capabilities()
		local lspkind = require("lspkind")

		local function lsp_setup(server, opts)
			lspconfig[server].setup(vim.tbl_deep_extend("force", {
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
		lsp_setup("intelephense", {
			init_options = {
				licenceKey = "00DRGKX774NA9NM",
			},
		})
		lsp_setup("rust_analyzer")
		lsp_setup("ruby_lsp")
		lsp_setup("jdtls")
		lsp_setup("volar")

		-- JetBrains Kotlin LSP (kotlin-lsp from homebrew)
		lspconfig.kotlin_language_server.setup({
			capabilities = capabilities,
			cmd = { "kotlin-lsp", "--stdio" },
			root_dir = lspconfig.util.root_pattern("settings.gradle", "settings.gradle.kts", "build.gradle", "build.gradle.kts", ".git"),
			filetypes = { "kotlin" },
		})

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
			experimental = { ghost_text = true },
		})
	end,
}
