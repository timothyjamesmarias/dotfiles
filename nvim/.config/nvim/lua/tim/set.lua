vim.cmd("syntax on")
vim.cmd("au FileType netrw setl bufhidden=wipe")
vim.api.nvim_set_var("netrw_fastbrowse", 0)
vim.opt.clipboard:append({ "unnamed", "unnamedplus" })
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
vim.opt.tags = 'tags'

