local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values
local entry_display = require("telescope.pickers.entry_display")
local previewers = require("telescope.previewers")
local Path = require("plenary.path")

local function scan_related_files(symbol)
  local files = {}

  -- Rails path helpers
  local function add_if_exists(label, path)
    if vim.fn.filereadable(path) == 1 then
      table.insert(files, { label = label, path = path })
    end
  end

  local function add_glob(label, pattern)
    local matches = vim.fn.glob(pattern, false, true)
    for _, match in ipairs(matches) do
      table.insert(files, { label = label, path = match })
    end
  end

  local lower = symbol:lower()
  local plural = lower .. "s"

  -- Heuristic paths
  add_if_exists("Model", "app/models/" .. lower .. ".rb")
  add_if_exists("Controller", "app/controllers/" .. plural .. "_controller.rb")
  add_glob("Migration", "db/migrate/*" .. lower .. "*.rb")
  add_glob("View", "app/views/" .. plural .. "/*.erb")
  add_if_exists("Test", "test/models/" .. lower .. "_test.rb")
  add_if_exists("Spec", "spec/models/" .. lower .. "_spec.rb")
  add_if_exists("Factory", "test/factories/" .. lower .. ".rb")
  add_if_exists("FactoryBot", "spec/factories/" .. lower .. ".rb")
  add_if_exists("Helper", "app/helpers/" .. plural .. "_helper.rb")

  -- rails.vim alternate file (if any)
  local alt = vim.fn["rails#buffer_variable"]("alternate")
  if alt and alt ~= "" and vim.fn.filereadable(alt) == 1 then
    table.insert(files, { label = "Alternate", path = alt })
  end

  return files
end

local function scan_instance_variable_usages(var)
  local results = {}
  local grep_results = vim.fn.systemlist("rg --vimgrep '@" .. var .. "\\s*=\\s*' app/")
  for _, line in ipairs(grep_results) do
    local filename, lnum, col, text = line:match("([^:]+):(%d+):(%d+):(.*)")
    if filename and lnum and col and text then
      table.insert(results, {
        label = "Instance Variable",
        path = filename,
        lnum = tonumber(lnum),
        col = tonumber(col),
        text = text,
      })
    end
  end
  return results
end

local function rails_related()
  local word = vim.fn.expand("<cword>")
  local related = {}

  if vim.startswith(word, "@") then
    local varname = word:sub(2)
    related = scan_instance_variable_usages(varname)
  else
    related = scan_related_files(word)
  end

  if #related == 0 then
    require("telescope.builtin").grep_string({ search = word })
    return
  end

  local displayer = entry_display.create({
    separator = " │ ",
    items = {
      { width = 18 }, -- label
      { remaining = true },
    },
  })

  local function make_display(entry)
    return displayer({
      entry.label,
      entry.path .. (entry.lnum and (":" .. entry.lnum) or ""),
    })
  end

  pickers.new({}, {
    prompt_title = "Rails Related Resources",
    finder = finders.new_table({
      results = related,
      entry_maker = function(entry)
        return {
          value = entry,
          display = make_display,
          ordinal = entry.path .. (entry.text or ""),
          path = entry.path,
          lnum = entry.lnum,
          col = entry.col,
          label = entry.label,
        }
      end,
    }),
    sorter = conf.generic_sorter({}),
    previewer = previewers.new_buffer_previewer({
      define_preview = function(self, entry, _)
        local p = Path:new(entry.path)
        if p:exists() then
          require("telescope.previewers.utils").job_maker({ filename = entry.path }, self.state)
          vim.schedule(function()
            if entry.lnum then
              vim.api.nvim_buf_call(self.state.bufnr, function()
                vim.api.nvim_win_set_cursor(self.state.winid, { entry.lnum, 0 })
                vim.cmd("normal! zz")
              end)
            end
          end)
        end
      end,
    }),
    attach_mappings = function(_, map)
      map("i", "<CR>", function(bufnr)
        local selection = require("telescope.actions.state").get_selected_entry()
        require("telescope.actions").close(bufnr)
        vim.cmd("edit " .. selection.path)
        if selection.lnum then
          vim.api.nvim_win_set_cursor(0, { selection.lnum, selection.col or 1 })
        end
      end)
      return true
    end,
  }):find()
end

return {
  rails_related = rails_related,
}
