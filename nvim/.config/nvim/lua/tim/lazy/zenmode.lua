return {
	"folke/zen-mode.nvim",
	opts = {
		window = {
			backdrop = 1,
			width = 120,
			options = {
			wrap = true,
			linebreak = true,
		},
		},
		plugins = {
			options = {
				enabled = true,
				ruler = true,
				showcmd = true,
				laststatus = 3,
			},
			twilight = { enabled = false },
			gitsigns = { enabled = true },
			tmux = { enabled = false },
			kitty = {
				enabled = false,
				font = "+4",
			},
			alacritty = {
				enabled = false,
				font = "14",
			},
		},
		on_open = function(win) end,
		on_close = function() end,
	},
}
