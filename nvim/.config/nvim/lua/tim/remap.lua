local tmux = require("tim.tmux_util")
local sessions = require("tim.sessions")

vim.g.mapleader = " "
vim.keymap.set("n", "<leader>w", "<cmd>w<CR>")
vim.keymap.set("n", ";", ":")
vim.keymap.set("i", "jj", "<Esc>", { silent = true })
vim.keymap.set("c", "jj", "<Esc>", { silent = true })
vim.keymap.set("n", "<leader>q", "<cmd>bp<bar>sp<bar>bn<bar>bd<CR>", { silent = true })
vim.keymap.set("n", "<leader>Q", "<cmd>q!<CR>", { silent = true })
vim.keymap.set("n", "n", "nzzzv", { silent = true })
vim.keymap.set("n", "N", "Nzzzv", { silent = true })
vim.keymap.set("n", "<leader>hh", "<cmd>vsp<CR>", { silent = true })
vim.keymap.set("n", "<leader>vv", "<cmd>sp<CR>", { silent = true })
vim.keymap.set("n", "<leader>sf", "/")
vim.keymap.set("i", "<C-e>", "<Esc>A")
vim.keymap.set("n", "<leader>cm", ":!")
vim.keymap.set("n", "<leader>ee", ":e ")
vim.keymap.set("n", "<leader>eb", ":Ebuf ")

vim.keymap.set("n", "<leader>lg", function()
	tmux.send_to_other_pane("lazygit")
end, { desc = "Open lazygit in other pane" })

function FZF_in_terminal()
	vim.cmd("botright 15split | terminal")
	local cmd = "find . -type f | grep -vE 'node_modules|target|.git' | fzf | xargs -r nvim"
	vim.fn.chansend(vim.b.terminal_job_id, cmd .. "\n")
end

vim.keymap.set("n", "<leader>ff", FZF_in_terminal, { desc = "FZF in terminal (same Neovim session)" })

-- sessions mappings
vim.keymap.set("n", "<leader>ss", sessions.save, { desc = "Save session" })
vim.keymap.set("n", "<leader>sr", sessions.load, { desc = "Restore session" })

-- git stuff
vim.keymap.set("n", "<leader>gcb", "<cmd>!tmux split-window -v 'gcb'<CR>")
vim.keymap.set("n", "<leader>gl", "<cmd>!tmux split-window -v 'glog'<CR>")

-- navigation
vim.keymap.set("n", "<C-n>", "<cmd>bnext<CR>", { silent = true, remap = true })
vim.keymap.set("n", "<C-p>", "<cmd>bprevious<CR>", { silent = true, remap = true })
vim.keymap.set("n", "<leader>ls", "<cmd>ls<CR>", { silent = true, remap = true })

vim.api.nvim_create_user_command("MapBufferKeys", function()
  -- Clear previous mappings
  for i = 1, 9 do
    pcall(vim.keymap.del, "n", "<leader>b" .. i)
  end

  -- Fetch currently listed buffers
  local buffers = vim.fn.getbufinfo({ buflisted = 1 })

  -- Set keymaps dynamically
  for i = 1, math.min(#buffers, 9) do
    local bufnr = buffers[i].bufnr
    vim.keymap.set("n", "<leader>b" .. i, function()
      vim.cmd("buffer " .. bufnr)
    end, { desc = "Go to buffer " .. bufnr })
  end
end, {})

vim.api.nvim_create_autocmd({ "BufAdd", "BufDelete" }, {
  callback = function()
    vim.cmd("MapBufferKeys")
  end,
})
