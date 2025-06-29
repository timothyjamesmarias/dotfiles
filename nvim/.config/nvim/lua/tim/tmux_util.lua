local M = {}

-- Get the current tmux pane ID
function M.get_current_pane()
  return vim.fn.system("tmux display-message -p '#{pane_id}'"):gsub("%s+", "")
end

-- Find another pane in the current tmux window
function M.get_other_pane()
  local current = M.get_current_pane()
  local panes = vim.fn.systemlist("tmux list-panes -F '#{pane_id}'")
  for _, p in ipairs(panes) do
    if p ~= current then return p end
  end
  return nil
end

function M.send_tmux_command(cmd, target)
  target = target or M.get_current_pane()
  if not target or target == "" then
    vim.notify("No valid tmux target found", vim.log.levels.WARN)
    return
  end
  local full = string.format("tmux send-keys -t %s %s C-m", target, vim.fn.shellescape(cmd))
  vim.notify("Sending to tmux: " .. full, vim.log.levels.INFO)
  vim.fn.system(full)
end

-- High-level shortcut: send to other pane
function M.send_to_other_pane(cmd)
  local pane = M.get_other_pane()
  if not pane then
    vim.notify("No other tmux pane found", vim.log.levels.WARN)
    return
  end
  M.send_tmux_command(cmd, pane)
end

-- Launch new nvim instance with a fuzzy-picked file
function M.launch_new_nvim_with_fzf()
  local pick = vim.fn.system("find . -type f | grep -vE 'node_modules|target|.git' | fzf"):gsub("%s+", "")
  if pick ~= "" then
    M.send_to_other_pane("nvim " .. vim.fn.shellescape(pick))
  end
end

return M
