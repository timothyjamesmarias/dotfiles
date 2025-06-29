vim.cmd("syntax on")
vim.cmd("au FileType netrw setl bufhidden=wipe")
vim.api.nvim_set_var("netrw_fastbrowse", 0)
vim.opt.clipboard:append({ "unnamed", "unnamedplus" })
vim.opt.path:append("**")
vim.opt.wildmenu = true
vim.opt.wildmode = { "longest:full", "full" }
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
vim.opt.tags = './tags,tags'
vim.g.skip_ts_context_commentstring_module = true
vim.opt.splitright = true

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = { "*.rb", "*.slim", "*.scss", "*.css", "*.js", "*.ts" },
  callback = function()
    vim.fn.jobstart({"ctags", "-R", "-f", "tags", "--options=.ctags", "."})
  end,
})

vim.wo.wrap = false

-- 🪄 Defer theme loading until plugins are ready
local theme_path = vim.fn.stdpath("config") .. "/lua/tim/theme.lua"
vim.schedule(function()
  local ok, result = pcall(dofile, theme_path)
  if not ok then
    vim.notify("Theme failed to load: " .. result, vim.log.levels.ERROR)
  end
end)

local socket_path = vim.fn.expand("~/.cache/nvim_socket")

vim.fn.mkdir(vim.fn.fnamemodify(socket_path, ":h"), "p")

