vim.cmd("syntax on")
vim.cmd("au FileType netrw setl bufhidden=wipe")
vim.api.nvim_set_var("netrw_fastbrowse", 0)
vim.opt.clipboard:append({ "unnamed", "unnamedplus" })
vim.opt.path:append("**")
vim.opt.wildmenu = true
vim.opt.wildmode = { "longest:full", "full" }
-- vim.opt.background = "dark"
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
vim.wo.wrap = false

-- vim.api.nvim_create_autocmd("BufWritePost", {
--   pattern = { "*.rb", "*.slim", "*.scss", "*.css", "*.js", "*.ts" },
--   callback = function()
--     vim.fn.jobstart({"ctags", "-R", "-f", "tags", "--options=.ctags", "."})
--   end,
-- })

function GitBranch()
  local handle = io.popen("git rev-parse --abbrev-ref HEAD 2>/dev/null")
  if handle then
    local result = handle:read("*l")
    handle:close()
    return result or ""
  end
  return ""
end

vim.o.statusline = "%f %m%r %=%{v:lua.GitBranch()} %l:%c [%p%%]"
