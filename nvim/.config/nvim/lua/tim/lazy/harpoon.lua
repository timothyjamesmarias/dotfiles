return {
	"ThePrimeagen/harpoon",
	config = function()
    local harpoon = require("harpoon")
    harpoon.setup({})
    -- vim.keymap.set("n", "<leader>hf", harpoon.add_file, { silent = true })
    -- vim.keymap.set("n", "<leader>hn", ui.nav_next, { silent = true })
    -- vim.keymap.set("n", "<leader>hp", require("harpoon.ui"), { silent = true })
    -- vim.keymap.set("n", "<leader>ht", require("harpoon.tmux"), { silent = true })
  end,
}
