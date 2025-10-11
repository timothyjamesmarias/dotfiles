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
			ensure_installed = {
				"lua_ls",
				"ruby_lsp",
				"ts_ls",
				"herb_ls",
				"html",
				"cssls",
				"vue_ls",
				"kotlin_language_server",
				"jdtls",
				"sqlls",
				"rust_analyzer",
			},
			automatic_installation = true,
		})

		-- LSP keymaps
		local function setup_lsp_keymaps(bufnr)
			local opts = { buffer = bufnr, silent = true, noremap = true }
			local telescope_builtin = require("telescope.builtin")

			-- Navigation (using Telescope for better UX)
			vim.keymap.set("n", "<leader>ld", telescope_builtin.lsp_definitions, opts)
			vim.keymap.set("n", "<leader>lD", vim.lsp.buf.declaration, opts)
			vim.keymap.set("n", "<leader>li", telescope_builtin.lsp_implementations, opts)
			vim.keymap.set("n", "<leader>lt", telescope_builtin.lsp_type_definitions, opts)
			vim.keymap.set("n", "<leader>lr", telescope_builtin.lsp_references, opts)

			-- Documentation
			vim.keymap.set("n", "<leader>lh", vim.lsp.buf.hover, opts)
			vim.keymap.set("n", "<leader>ls", vim.lsp.buf.signature_help, opts)

			-- Code actions
			vim.keymap.set("n", "<leader>la", vim.lsp.buf.code_action, opts)
			vim.keymap.set("n", "<leader>ln", vim.lsp.buf.rename, opts)
			-- Use formatter.nvim instead of LSP formatting
			vim.keymap.set("n", "<leader>lf", "<cmd>Format<CR>", opts)

			-- Diagnostics (using Telescope for diagnostics lists)
			vim.keymap.set("n", "<leader>le", vim.diagnostic.open_float, opts)
			vim.keymap.set("n", "<leader>lq", telescope_builtin.diagnostics, opts)
			vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
			vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)

			-- Workspace
			vim.keymap.set("n", "<leader>lwa", vim.lsp.buf.add_workspace_folder, opts)
			vim.keymap.set("n", "<leader>lwr", vim.lsp.buf.remove_workspace_folder, opts)
			vim.keymap.set("n", "<leader>lwl", function()
				print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
			end, opts)

			-- Document symbols (using Telescope)
			vim.keymap.set("n", "<leader>lo", telescope_builtin.lsp_document_symbols, opts)
			vim.keymap.set("n", "<leader>lO", telescope_builtin.lsp_workspace_symbols, opts)

			-- Incoming/Outgoing calls
			vim.keymap.set("n", "<leader>lci", telescope_builtin.lsp_incoming_calls, opts)
			vim.keymap.set("n", "<leader>lco", telescope_builtin.lsp_outgoing_calls, opts)

			-- LSP control
			vim.keymap.set("n", "<leader>lR", "<cmd>LspRestart<CR>", opts)
			vim.keymap.set("n", "<leader>lI", "<cmd>LspInfo<CR>", opts)
		end

		local function lsp_setup(server, opts)
			lspconfig[server].setup(vim.tbl_deep_extend("force", {
				on_attach = function(client, bufnr)
					setup_lsp_keymaps(bufnr)
					if on_attach then
						on_attach(client, bufnr)
					end
				end,
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
		lsp_setup("kotlin_language_server")
		lsp_setup("jdtls")
		lsp_setup("volar")

		-- Herb LSP (if you’ve installed @herb-tools/language-server)
		-- npm i -g @herb-tools/language-server
		-- If Mason doesn’t manage it, just start it via lspconfig:
		if lspconfig.herb_ls then
			lsp_setup("herb_ls", { filetypes = { "eruby" } })
		end

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
