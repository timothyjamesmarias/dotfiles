vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- remaps
vim.g.mapleader = " "
vim.keymap.set("n", "<leader>w", "<cmd>w<CR>")
vim.keymap.set("n", ";", ":")
vim.keymap.set("i", "jj", "<Esc>", { silent = true })
vim.keymap.set("n", "<C-n>", "<cmd>bnext<CR>", { silent = true, remap = true })
vim.keymap.set("n", "<C-p>", "<cmd>bprevious<CR>", { silent = true, remap = true })
vim.keymap.set("n", "<leader>q", "<cmd>bd<CR>", { silent = true })
vim.keymap.set("n", "<leader>Q", "<cmd>q!<CR>", { silent = true })
vim.keymap.set("n", "n", "nzzzv", { silent = true })
vim.keymap.set("n", "N", "Nzzzv", { silent = true })
vim.keymap.set("n", "<leader>hh", "<cmd>vsp<CR>", { silent = true })
vim.keymap.set("n", "<leader>vv", "<cmd>sp<CR>", { silent = true })
vim.keymap.set("n", "<leader>sf", "/")

-- plugins
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	{
		"catppuccin/nvim",
		name = "catppuccin",
		priority = 1000,
		config = function()
			vim.cmd([[colorscheme catppuccin-mocha]])
		end,
	},
	{
		"nvim-tree/nvim-tree.lua",
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			local nvim_tree = require("nvim-tree")
			nvim_tree.setup({
				filters = {
					dotfiles = true,
				},
				view = {
					width = 70,
				},
			})
			vim.keymap.set("n", "<leader>nn", "<cmd>NvimTreeToggle<CR>", { silent = true })
			vim.keymap.set("n", "<leader>ns", "<cmd>NvimTreeFindFile<CR>", { silent = true })
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		event = { "BufReadPre", "BufNewFile" },
		build = "<cmd>TSUpdate",
		dependencies = {
			"windwp/nvim-ts-autotag",
			"JoosepAlviste/nvim-ts-context-commentstring",
			"EmranMR/tree-sitter-blade",
			"RRethy/nvim-treesitter-endwise",
			"vrischmann/tree-sitter-templ",
		},
		config = function()
			local treesitter = require("nvim-treesitter.configs")
			treesitter.setup({
				highlight = { enable = true, additional_vim_regex_highlighting = false },
				modules = {},
				indent = { enable = true },
				autotag = { enable = true },
				ensure_installed = "all",
				auto_install = true,
				context_commentstring = {
					enable = true,
				},
				endwise = {
					enable = true,
				},
			})
			vim.filetype.add({
				extension = {
					templ = "templ",
				},
			})
		end,
	},
	{
		"windwp/nvim-ts-autotag",
		dependencies = "nvim-treesitter/nvim-treesitter",
		config = function()
			require("nvim-ts-autotag").setup({
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
			})
		end,
		lazy = true,
		event = "VeryLazy",
	},
	{
		"stevearc/aerial.nvim",
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			local aerial = require("aerial")
			aerial.setup()
		end,
	},
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.3",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-file-browser.nvim",
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make",
			},
			"nvim-treesitter/nvim-treesitter",
			"stevearc/aerial.nvim",
			"debugloop/telescope-undo.nvim",
			"nvim-telescope/telescope-ui-select.nvim",
		},
		config = function()
			local telescope = require("telescope")
			telescope.setup({
				defaults = {
					layout_config = {
						prompt_position = "top",
						width = 0.9,
						height = 0.6,
					},
					sorting_strategy = "ascending",
					border = true,
					hidden = true,
					file_ignore_patterns = {
						".git/",
					},
				},
				pickers = {},
				extensions = {
					fzf = {
						fuzzy = true,
						override_generic_sorter = true,
						override_file_sorter = true,
						case_mode = "smart_case",
					},
					aerial = {},
					undo = {},
					file_browser = {},
					ui_select = {
						require("telescope.themes").get_cursor({
							results_title = false,
							previewer = false,
							border = true,
						}),
					},
				},
			})
			telescope.load_extension("fzf")
			telescope.load_extension("file_browser")
			telescope.load_extension("aerial")
			telescope.load_extension("undo")
			telescope.load_extension("ui-select")

			local builtin = require("telescope.builtin")
			vim.keymap.set(
				"n",
				"<leader>ff",
				"<cmd>Telescope find_files follow=true hidden=true <CR>",
				{ silent = true }
			)
			vim.keymap.set("n", "<leader>fr", builtin.oldfiles, { silent = true })
			vim.keymap.set("n", "<leader>fg", builtin.git_commits, { silent = true })
			vim.keymap.set("n", "<leader>fw", builtin.live_grep, { silent = true })
			vim.keymap.set("n", "<leader>fk", builtin.keymaps, { silent = true })
			vim.keymap.set("n", "<leader>fp", builtin.pickers, { silent = true })
			vim.keymap.set("n", "<leader>fcc", builtin.commands, { silent = true })
			vim.keymap.set("n", "<leader>fca", builtin.autocommands, { silent = true })
			vim.keymap.set("n", "<leader>fb", builtin.buffers, { silent = true })
			vim.keymap.set("n", "<leader>fh", builtin.help_tags, { silent = true })
			vim.keymap.set("n", "<leader>sp", builtin.spell_suggest, { silent = true })
			vim.keymap.set("n", "<leader>fn", "<cmd>Telescope file_browser<CR>", { silent = true, noremap = true })
			vim.keymap.set("n", "<leader>fa", "<cmd>Telescope aerial<CR>", { silent = true, noremap = true })
			vim.keymap.set("n", "<leader>tg", builtin.tagstack, { silent = true })
			vim.keymap.set("n", "<leader>sl", builtin.grep_string, { silent = true, noremap = true })
			vim.keymap.set(
				"v",
				"<leader>sl",
				"y<ESC>:Telescope grep_string default_text=<c-r>0<CR>",
				{ silent = true, noremap = true }
			)
			vim.keymap.set("n", "<leader>u", "<cmd>Telescope undo<CR>")
		end,
	},

	-- lazy.nvim
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		opts = {
			-- add any options here
		},
		dependencies = {
			-- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
			"MunifTanjim/nui.nvim",
		},
	},
	{
		"williamboman/mason.nvim",
		dependencies = {
			"williamboman/mason-lspconfig.nvim",
		},
		config = function()
			local mason = require("mason")
			local mason_lspconfig = require("mason-lspconfig")
			mason.setup()
			mason_lspconfig.setup()
			vim.keymap.set("n", "<leader>M", "<cmd>Mason<CR>")
		end,
	},
	{
		"tpope/vim-commentary",
	},
	{
		"tpope/vim-surround",
	},
	{
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
				filetypes = { "html", "eruby", "blade" },
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
					"slim",
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
				},
			})
			lspconfig["cssls"].setup({
				on_attach = on_attach,
				capabilities = capabilities,
				filetypes = {
					"html",
					"css",
					"scss",
					"less",
					"eruby",
					"blade",
					"vue",
					"javascript",
					"typescript",
					"javascriptreact",
					"typescriptreact",
				},
			})
			lspconfig["solargraph"].setup({
				on_attach = on_attach,
				capabilities = capabilities,
				filetypes = { "ruby" },
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
			lspconfig["rust_analyzer"].setup({
				on_attach = on_attach,
				capabilities = capabilities,
			})
			lspconfig["gopls"].setup({
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
	},
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		opts = {},
	},
	{
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
					lualine_c = { "filename" },
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
	},
	{
		"akinsho/bufferline.nvim",
		version = "*",
		dependencies = "nvim-tree/nvim-web-devicons",
		config = function()
			local bufferline = require("bufferline")
			vim.opt.termguicolors = true
			bufferline.setup({
				options = {
					diagnostics = "nvim_lsp",
					diagnostics_indicator = function(count, level)
						local icon = level:match("error") and " " or " "
						return " " .. icon .. count
					end,
					tab_size = 20,
				},
			})
		end,
	},
	{
		"lewis6991/gitsigns.nvim",
		config = function()
			local gitsigns = require("gitsigns")
			gitsigns.setup({
				current_line_blame = true,
				current_line_blame_opts = {
					virt_text = true,
					virt_text_pos = "eol",
					delay = 0,
					ignore_whitespace = false,
				},
			})
		end,
	},
	{
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
						exe = "gofmt",
					},
					["*"] = {
						require("formatter.filetypes.any").remove_trailing_whitespace,
					},
				},
			})
			vim.api.nvim_create_autocmd("BufWritePost", {
				command = "Format",
			})
			vim.keymap.set("n", "<leader>fm", "<cmd>Format<CR>", { silent = false })
		end,
	},
	{
		"rmagatti/session-lens",
		dependencies = {
			"rmagatti/auto-session",
			"nvim-telescope/telescope.nvim",
			"nvim-lualine/lualine.nvim",
		},
		config = function()
			local auto_session = require("auto-session")
			local session_lens = require("session-lens")
			auto_session.setup({
				log_level = "error",
				auto_session_suppress_dirs = { "~/", "~/projects", "~/Downloads", "/" },
				auto_session_use_git_branch = true,
				auto_restore_enabled = true,
				auto_session_root_dir = vim.fn.stdpath("data") .. "/sessions/",
				auto_session_enabled = true,
				auto_session_create_enabled = true,
				post_cwd_changed_hook = function()
					require("lualine").refresh()
				end,
			})
			session_lens.setup({})
			vim.keymap.set("n", "<leader>se", "<cmd>Telescope session-lens search_session<CR>")
		end,
	},
	{
		"alexghergh/nvim-tmux-navigation",
		config = function()
			local nvim_tmux_nav = require("nvim-tmux-navigation")
			nvim_tmux_nav.setup({
				disable_when_zoomed = true,
				keybindings = {
					left = "<C-h>",
					down = "<C-j>",
					up = "<C-k>",
					right = "<C-l>",
					last_active = "<C-\\>",
					next = "<C-Space>",
				},
			})
		end,
	},
	{
		"mfussenegger/nvim-dap",
		dependencies = {
			"rcarriga/nvim-dap-ui",
		},
		enabled = true,
		config = function()
			local dap = require("dap")
			local dapui = require("dapui")
			dapui.setup()
			dap.listeners.before.attach.dapui_config = function()
				dapui.open()
			end
			dap.listeners.before.launch.dapui_config = function()
				dapui.open()
			end
			dap.listeners.before.event_terminated.dapui_config = function()
				dapui.close()
			end
			dap.listeners.before.event_exited.dapui_config = function()
				dapui.close()
			end
			vim.keymap.set("n", "<leader>b", ":lua require('dap').toggle_breakpoint()<CR>", {})
			vim.keymap.set("n", "<leader>dc", ":lua require('dap').continue()<CR>", {})
			vim.keymap.set("n", "<leader>dq", ":lua require('dap').terminate()<CR>", {})
			vim.keymap.set("n", "<leader>do", ":lua require('dap').step_over()<CR>", {})
			vim.keymap.set("n", "<leader>dp", ":lua require('dap').pause()<CR>", {})
			vim.keymap.set("n", "<leader>di", ":lua require('dap').step_into()<CR>", {})
			vim.keymap.set("n", "<leader>dr", ":lua require('dap').reverse_continue()<CR>", {})
		end,
	},
	{
		"leoluz/nvim-dap-go",
		config = function()
			local dap_go = require("dap-go")
			dap_go.setup()
		end,
	},
	{
		"nvim-neotest/neotest",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"antoinemadec/FixCursorHold.nvim",
			"nvim-treesitter/nvim-treesitter",
			"nvim-neotest/neotest-go",
		},
		config = function()
			local neotest = require("neotest")
			neotest.setup({
				adapters = {
					require("neotest-go"),
				},
			})
			vim.keymap.set("n", "<leader>tt", ":lua require('neotest').run.run()<CR>", {})
			vim.keymap.set("n", "<leader>tl", ":lua require('neotest').run.run_last()<CR>", {})
			vim.keymap.set("n", "<leader>tf", ":lua require('neotest').run.run(vim.fn.expand('%'))<CR>", {})
			vim.keymap.set("n", "<leader>ta", ":lua require('neotest').run.run(vim.fn.getcwd())<CR>", {})
			vim.keymap.set("n", "<leader>ts", ":lua require('neotest').run.stop()<CR>", {})
			vim.keymap.set("n", "<leader>tw", ":lua require('neotest').watch.toggle()<CR>", {})
		end,
	},
})

-- options
vim.cmd("syntax on")
vim.cmd("au FileType netrw setl bufhidden=wipe")
vim.api.nvim_set_var("netrw_fastbrowse", 0)
vim.opt.clipboard = "unnamedplus" --requires a clipboard manager like xclip
vim.opt.background = "dark"
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.smartindent = true
vim.opt.mouse = a
vim.opt.spelllang = "en_us"
vim.opt.spell = true
vim.opt.updatetime = 1000
vim.opt.completeopt = { "menuone", "longest", "preview" }
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

vim.opt.scrolloff = 8
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.termguicolors = true
