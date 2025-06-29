local M = {}

-- Get session path using cwd and optionally the current git branch
function M.get_session_path()
  local cwd = vim.fn.fnamemodify(vim.fn.getcwd(), ":p:h:t")
  local branch = vim.fn.system("git rev-parse --abbrev-ref HEAD 2>/dev/null"):gsub("%s+", "")
  if branch ~= "" then
    cwd = cwd .. "__" .. branch
  end
  return vim.fn.stdpath("data") .. "/sessions/" .. cwd .. ".vim"
end

function M.save()
  local path = M.get_session_path()
  vim.cmd("mksession! " .. vim.fn.fnameescape(path))
  vim.notify("Session saved: " .. path)
end

function M.load()
  local path = M.get_session_path()
  if vim.fn.filereadable(path) == 1 then
    vim.cmd("source " .. vim.fn.fnameescape(path))
    vim.notify("Session loaded: " .. path)
  else
    vim.notify("No session found at " .. path, vim.log.levels.WARN)
  end
end

return M
