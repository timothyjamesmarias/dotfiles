vim.g.mapleader = " "
vim.keymap.set("n", "<leader>w", "<cmd>w<CR>")
vim.keymap.set("n", ";", ":")
vim.keymap.set("i", "jj", "<Esc>", { silent = true })
vim.keymap.set("c", "jj", "<Esc>", { silent = true })
vim.keymap.set("n", "<C-n>", "<cmd>bnext<CR>", { silent = true, remap = true })
vim.keymap.set("n", "<C-p>", "<cmd>bprevious<CR>", { silent = true, remap = true })
vim.keymap.set("n", "<leader>q", "<cmd>bp<bar>sp<bar>bn<bar>bd<CR>", { silent = true })
vim.keymap.set("n", "<leader>Q", "<cmd>q!<CR>", { silent = true })
vim.keymap.set("n", "n", "nzzzv", { silent = true })
vim.keymap.set("n", "N", "Nzzzv", { silent = true })
vim.keymap.set("n", "<leader>hh", "<cmd>vsp<CR>", { silent = true })
vim.keymap.set("n", "<leader>vv", "<cmd>sp<CR>", { silent = true })
vim.keymap.set("n", "<leader>sf", "/")

vim.keymap.set("i", "<C-e>", "<Esc>A")
vim.keymap.set("n", "<leader>er", "oif err != nil {<CR>}<Esc>")
vim.keymap.set("n", "<leader>cm", ":!")
vim.keymap.set("n", "<leader>ee", ":e ")
vim.keymap.set("n", "<leader>x", ":! chmod +x %<CR>")

vim.keymap.set("n", "<leader>nn", "<cmd>Explore<CR>")
vim.keymap.set("n", "<leader>ns", "<cmd>Explore %:p:h<CR>")

vim.keymap.set("n", "<C-b>", ":! tmux neww tmux-sessionizer<CR>", { silent = true })

