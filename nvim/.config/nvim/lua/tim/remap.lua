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
vim.keymap.set("n", "<leader>cm", ":!")
vim.keymap.set("n", "<leader>ee", ":e ")

vim.api.nvim_create_autocmd("FileType", {
  pattern = "copilot-chat", -- Change to your actual Copilot Chat filetype
  callback = function()
    vim.api.nvim_buf_set_keymap(0, "n", "<leader>w", ":CopilotChatSave", { noremap = true, silent = true })
  end,
})
