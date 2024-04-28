return {
  "tpope/vim-fugitive",
  config = function ()
    vim.keymap.set("n", "<leader>gg", "<cmd>Git<CR>", { silent = true })
    vim.keymap.set("n", "<leader>gw", "<cmd>Gwrite<CR>", { silent = true })
    vim.keymap.set("n", "<leader>gv", "<cmd>Git difftool<CR>", { silent = true })
    vim.keymap.set("n", "<leader>gm", "<cmd>Git mergetool<CR>", { silent = true })
    vim.keymap.set("n", "<leader>gs", ":Gclog -S", { silent = true })
  end
}
