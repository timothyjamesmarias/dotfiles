return {
	"nvim-telescope/telescope.nvim",
	branch = "master",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-telescope/telescope-file-browser.nvim",
		{
			"nvim-telescope/telescope-fzf-native.nvim",
			build = "make",
		},
		"nvim-treesitter/nvim-treesitter",
		"nvim-telescope/telescope-ui-select.nvim",
	},
	config = function()
		local telescope = require("telescope")
		local actions = require("telescope.actions")
		local sorters = require("telescope.sorters")
		local conf = require("telescope.config").values

		-- Context-aware file type priorities
		-- Lower number = higher priority (will be shown first)
		local filetype_priorities = {
			-- When in Ruby files
			ruby = {
				ruby = 1,
				eruby = 2,
				slim = 2,
				haml = 2,
				yml = 3,
				yaml = 3,
				css = 4,
				scss = 4,
				sass = 4,
				rake = 5, -- Rake files lower priority (config/tasks)
				javascript = 6,
				typescript = 6,
			},
			-- When in ERB/Slim templates
			eruby = {
				ruby = 1,
				eruby = 2,
				slim = 2,
				haml = 2,
				css = 3,
				scss = 3,
				sass = 3,
				javascript = 4,
				typescript = 4,
				rake = 5, -- Rarely need rake from templates
			},
			slim = {
				ruby = 1,
				slim = 2,
				eruby = 2,
				haml = 2,
				css = 3,
				scss = 3,
				sass = 3,
				javascript = 4,
				typescript = 4,
				rake = 5,
			},
			-- When in PHP files
			php = {
				php = 1,
				blade = 2,
				css = 3,
				scss = 3,
				javascript = 4,
				typescript = 4,
			},
			-- When in Blade templates
			blade = {
				php = 1,
				blade = 2,
				css = 3,
				scss = 3,
				javascript = 4,
				typescript = 4,
			},
			-- When in JavaScript/TypeScript
			javascript = {
				javascript = 1,
				typescript = 1,
				jsx = 1,
				tsx = 1,
				css = 2,
				scss = 2,
				html = 3,
				eruby = 4,
			},
			typescript = {
				typescript = 1,
				javascript = 1,
				tsx = 1,
				jsx = 1,
				css = 2,
				scss = 2,
				html = 3,
				eruby = 4,
			},
			-- When in CSS/SCSS
			css = {
				css = 1,
				scss = 1,
				sass = 1,
				html = 2,
				eruby = 2,
				slim = 2,
				javascript = 3,
			},
			scss = {
				scss = 1,
				css = 1,
				sass = 1,
				html = 2,
				eruby = 2,
				slim = 2,
				javascript = 3,
			},
			-- When in HTML (e.g., Maizzle projects)
			html = {
				html = 1,
				css = 2,
				scss = 2,
				javascript = 3,
				typescript = 3,
			},
		}

		-- Helper: Get file extension from path
		local function get_file_extension(path)
			if not path then
				return nil
			end
			-- Handle compound extensions like .html.erb
			if path:match("%.html%.erb$") or path:match("%.erb$") then
				return "eruby"
			elseif path:match("%.blade%.php$") then
				return "blade"
			end
			return path:match("^.+%.([^%.]+)$")
		end

		-- Helper: Map extension to filetype
		local extension_to_filetype = {
			rb = "ruby",
			rake = "rake", -- Treat rake files separately (lower priority)
			erb = "eruby",
			slim = "slim",
			haml = "haml",
			php = "php",
			blade = "blade",
			js = "javascript",
			jsx = "javascript",
			ts = "typescript",
			tsx = "typescript",
			css = "css",
			scss = "scss",
			sass = "sass",
			yml = "yaml",
			yaml = "yaml",
			html = "html",
		}

		-- Custom sorter that boosts results based on file type context
		local function context_aware_sorter(opts)
			opts = opts or {}

			-- Lazy load the fzf sorter only when needed
			local fzf_sorter
			local function get_fzf_sorter()
				if not fzf_sorter then
					local status, result = pcall(require("telescope.sorters").get_fzf_sorter, opts)
					if status then
						fzf_sorter = result
					else
						-- Fallback to generic sorter if fzf not available
						fzf_sorter = require("telescope.sorters").get_generic_fuzzy_sorter(opts)
					end
				end
				return fzf_sorter
			end

			-- Cache for file type lookups (cleared on each new search)
			local filetype_cache = {}
			local current_ft_cache = nil

			return sorters.Sorter:new({
				discard = function(...)
					return get_fzf_sorter():discard(...)
				end,

				scoring_function = function(_, prompt, entry)
					local base_sorter = get_fzf_sorter()

					-- Get base score from fzf
					local score = base_sorter:scoring_function(prompt, entry)

					-- If feature is disabled or no valid score, return as-is
					if _G.context_sort_enabled == false or not score or score == -1 then
						return score
					end

					-- Cache current buffer filetype (doesn't change during search)
					if not current_ft_cache then
						current_ft_cache = vim.bo.filetype
					end
					local current_ft = current_ft_cache

					-- Check if we have priorities for current filetype
					local priorities = filetype_priorities[current_ft]
					if not priorities then
						-- No priorities defined for this filetype, use default fzf sorting
						return score
					end

					-- Get result file extension/type
					local result_path = entry.filename or entry.value or entry.path
					if not result_path then
						return score
					end

					-- Cache file type lookups
					local result_ft = filetype_cache[result_path]
					if not result_ft then
						local result_ext = get_file_extension(result_path)
						result_ft = extension_to_filetype[result_ext] or result_ext
						filetype_cache[result_path] = result_ft
					end

					if not result_ft then
						return score
					end

					-- Get priority boost (lower priority number = bigger boost)
					local priority = priorities[result_ft]
					if priority then
						-- Apply boost: multiply score by (priority * 0.5)
						-- Priority 1 → 0.5x score (50% boost, shows first)
						-- Priority 2 → 1.0x score (no change)
						-- Priority 3 → 1.5x score (appears later)
						-- Priority 5 → 2.5x score (appears much later)
						local boost_factor = priority * 0.5
						score = score * boost_factor
					end
					-- NOTE: Removed penalty for files not in priority list
					-- This prevents unrelated files from being hidden entirely

					return score
				end,

				highlighter = function(...)
					return get_fzf_sorter():highlighter(...)
				end,
			})
		end

		-- Store the custom sorter globally so we can use it in pickers
		_G.context_aware_sorter = context_aware_sorter

		telescope.setup({
			defaults = {
				vimgrep_arguments = {
					"rg",
					"--color=never",
					"--no-heading",
					"--with-filename",
					"--line-number",
					"--column",
					"--smart-case",
					"--hidden",
					"--glob=!.git/",
					"--fixed-strings", -- Literal string search (not regex)
				},
				mappings = {
					n = {
						["q"] = actions.close,
					},
					i = {},
				},
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
			pickers = {
				-- Apply context-aware sorting to specific pickers
				-- NOTE: Disabled for grep_string and live_grep - the custom sorter causes
				-- issues with large result sets (e.g., HTML files in Maizzle projects)
				-- Keep it enabled for tags and LSP where result sets are smaller
				tags = {
					sorter = context_aware_sorter(),
				},
				-- LSP pickers (these benefit from context awareness)
				lsp_references = {
					sorter = context_aware_sorter(),
				},
				lsp_definitions = {
					sorter = context_aware_sorter(),
				},
				lsp_implementations = {
					sorter = context_aware_sorter(),
				},
				-- DON'T apply to file finding - use default fast sorter
				-- find_files uses fzf by default which is already very fast
				buffers = {
					mappings = {
						i = {
							["<C-x>"] = actions.delete_buffer,
						},
						n = {
							["<C-x>"] = actions.delete_buffer,
						},
					},
				},
			},
			extensions = {
				fzf = {
					fuzzy = true,
					override_generic_sorter = true,
					override_file_sorter = true,
					case_mode = "smart_case",
				},
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

		pcall(telescope.load_extension, "fzf")
		pcall(telescope.load_extension, "file_browser")
		pcall(telescope.load_extension, "ui-select")

		-- Create user command to toggle context-aware sorting
		vim.api.nvim_create_user_command("ToggleContextSort", function()
			_G.context_sort_enabled = not _G.context_sort_enabled
			local status = _G.context_sort_enabled and "enabled" or "disabled"
			print("Context-aware sorting " .. status)

			-- Reload telescope config
			package.loaded["telescope"] = nil
			require("telescope").setup(telescope.setup)
		end, { desc = "Toggle context-aware sorting in Telescope" })

		-- Enable by default
		_G.context_sort_enabled = true
	end,
}
