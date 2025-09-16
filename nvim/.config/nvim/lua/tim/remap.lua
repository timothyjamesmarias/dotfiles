vim.g.mapleader = " "
vim.keymap.set("n", "<leader>w", "<cmd>w<CR>")
vim.keymap.set("n", ";", ":")
vim.keymap.set("n", "<leader>q", "<cmd>bp<bar>sp<bar>bn<bar>bd<CR>", { silent = true })
vim.keymap.set("n", "<leader>Q", "<cmd>q!<CR>", { silent = true })
vim.keymap.set("n", "n", "nzzzv", { silent = true })
vim.keymap.set("n", "N", "Nzzzv", { silent = true })
vim.keymap.set("n", "<leader>hh", "<cmd>vsp<CR>", { silent = true })
vim.keymap.set("n", "<leader>vv", "<cmd>sp<CR>", { silent = true })
vim.keymap.set("n", "<leader>sf", "/")
vim.keymap.set("i", "<C-e>", "<C-o>A")
vim.keymap.set("n", "<leader>cm", ":!")
vim.keymap.set("n", "<leader>ee", ":e ")
vim.keymap.set("n", "<leader>eb", ":Ebuf ")
vim.keymap.set("i", "<C-l>", "<C-o>l")

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
