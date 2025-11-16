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
		local capabilities = require("cmp_nvim_lsp").default_capabilities()
		local lspkind = require("lspkind")

		-- Helper function to create LSP configs
		local function make_config(opts)
			return vim.tbl_deep_extend("force", {
				capabilities = capabilities,
			}, opts or {})
		end

		-- LSP servers using new vim.lsp.config API
		vim.lsp.config.html = make_config({ filetypes = { "html", "eruby", "blade" } })
		vim.lsp.config.cssls = make_config({ filetypes = { "html", "css", "scss" } })
		vim.lsp.config.ts_ls = make_config({
			filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" }
		})
		vim.lsp.config.lua_ls = make_config({
			settings = {
				Lua = {
					runtime = { version = "LuaJIT" },
					diagnostics = { globals = { "vim", "require" } },
					workspace = { library = vim.api.nvim_get_runtime_file("", true) },
					telemetry = { enable = false },
				},
			},
		})
		vim.lsp.config.clangd = make_config({ filetypes = { "c", "cpp" } })
		vim.lsp.config.sqlls = make_config({ filetypes = { "sql", "mysql", "pgsql" } })
		vim.lsp.config.intelephense = make_config({
			init_options = {
				licenceKey = "00DRGKX774NA9NM",
			},
		})
		vim.lsp.config.rust_analyzer = make_config({})
		vim.lsp.config.ruby_lsp = make_config({})
		vim.lsp.config.jdtls = make_config({})
		vim.lsp.config.vue_ls = make_config({})
		vim.lsp.config.tailwindcss = make_config({
			filetypes = { "html", "css", "scss", "javascript", "javascriptreact", "typescript", "typescriptreact", "vue", "eruby" },
		})

		-- Enable LSP servers
		vim.lsp.enable({
			"html",
			"cssls",
			"ts_ls",
			"tailwindcss",
			"lua_ls",
			"clangd",
			"sqlls",
			"intelephense",
			"rust_analyzer",
			"ruby_lsp",
			"vue_ls",
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
