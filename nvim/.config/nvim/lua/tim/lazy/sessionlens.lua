return {
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
	}
