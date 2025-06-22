local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values
local entry_display = require("telescope.pickers.entry_display")
local utils = require("telescope.utils")

-- === Display & Navigation ===

local function format_location(item)
	local uri = item.uri or item.targetUri
	local range = item.range or item.targetSelectionRange
	local filename = vim.uri_to_fname(uri)
	local row = range.start.line + 1
	local col = range.start.character + 1
	local bufnr = vim.fn.bufnr(filename, true)
	local line = vim.fn.getbufline(bufnr, row)[1] or ""
	return {
		filename = filename,
		lnum = row,
		col = col,
		text = vim.trim(line),
		source = item._source or "unknown",
	}
end

local function jump_to_location(loc)
	vim.cmd("edit " .. loc.filename)
	vim.api.nvim_win_set_cursor(0, { loc.lnum, loc.col - 1 })
end

-- === Ranking Function ===

local function score_entry(entry)
	local source_weight = ({
		Definition = 100,
		TypeDefinition = 90,
		Implementation = 80,
		Tag = 50,
		Grep = 30,
	})[entry.source] or 0

	local current_file = vim.api.nvim_buf_get_name(0)
	local same_file_bonus = (entry.filename == current_file) and 20 or 0

	local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
	local distance = math.abs((entry.lnum or 0) - cursor_line)
	local distance_penalty = -math.min(distance, 10) -- cap penalty

	return source_weight + same_file_bonus + distance_penalty
end

-- === Telescope Picker ===

local function create_dwim_picker(results)
	local displayer = entry_display.create({
		separator = " │ ",
		items = {
			{ width = 15 }, -- Source
			{ remaining = true },
		},
	})

	local function make_display(entry)
		return displayer({
			entry.source,
			entry.filename .. ":" .. entry.lnum .. " → " .. entry.text,
		})
	end

	pickers
		.new({}, {
			prompt_title = "DWIM Definition (Ranked)",
			finder = finders.new_table({
				results = results,
				entry_maker = function(entry)
					return {
						value = entry,
						display = make_display,
						ordinal = entry.filename .. entry.text,
						filename = entry.filename,
						lnum = entry.lnum,
						col = entry.col,
						source = entry.source,
					}
				end,
			}),
			sorter = conf.generic_sorter({}),
			previewer = require("telescope.previewers").new_buffer_previewer({
				define_preview = function(self, entry, status)
					local filename = entry.filename or entry.value.filename
					local lnum = entry.lnum or entry.value.lnum or 1

					require("telescope.previewers.utils").job_maker({
						filename = filename,
					}, self.state)

					vim.schedule(function()
						vim.api.nvim_buf_call(self.state.bufnr, function()
							vim.api.nvim_win_set_cursor(self.state.winid, { lnum, 0 })
							vim.cmd("normal! zz")
						end)
					end)
				end,
			}),
			attach_mappings = function(_, map)
				map("i", "<CR>", function(bufnr)
					local selection = require("telescope.actions.state").get_selected_entry()
					require("telescope.actions").close(bufnr)
					jump_to_location(selection.value)
				end)
				return true
			end,
		})
		:find()
end

-- === LSP & Tags Aggregation ===

local function fetch_lsp(kind)
	return vim.lsp.buf_request_sync(0, kind, vim.lsp.util.make_position_params(), 500) or {}
end

local function flatten_lsp_results(results, source)
	local locations = {}
	for _, res in pairs(results) do
		local result = res.result
		if result then
			if vim.islist(result) then
				for _, loc in ipairs(result) do
					loc._source = source
					table.insert(locations, format_location(loc))
				end
			else
				result._source = source
				table.insert(locations, format_location(result))
			end
		end
	end
	return locations
end

-- === Main Entry ===

local function run()
	local word = vim.fn.expand("<cword>")
	local items = {}

	-- 🔍 Handle instance variables like @user
	if vim.startswith(word, "@") then
		local var = word:sub(2) -- strip '@'
		local grep_results = vim.fn.systemlist("rg --vimgrep '@" .. var .. "\\s*=\\s*' app/")
		for _, line in ipairs(grep_results) do
			local filename, lnum, col, text = line:match("([^:]+):(%d+):(%d+):(.*)")
			if filename and lnum and col and text then
				table.insert(items, {
					filename = filename,
					lnum = tonumber(lnum),
					col = tonumber(col),
					text = text,
					source = "Instance Variable",
				})
			end
		end

		if #items > 0 then
			table.sort(items, function(a, b)
				return score_entry(a) > score_entry(b)
			end)
			create_dwim_picker(items)
			return
		end
	end

	-- LSP results
	vim.list_extend(items, flatten_lsp_results(fetch_lsp("textDocument/definition"), "Definition"))
	vim.list_extend(items, flatten_lsp_results(fetch_lsp("textDocument/typeDefinition"), "TypeDefinition"))
	vim.list_extend(items, flatten_lsp_results(fetch_lsp("textDocument/implementation"), "Implementation"))

	-- Tags
	local tags = vim.fn.taglist(word)
	for _, tag in ipairs(tags) do
		table.insert(items, {
			filename = tag.filename,
			lnum = tonumber(tag.cmd:match("(%d+)")),
			col = 1,
			text = tag.name,
			source = "Tag",
		})
	end

	-- Final fallback: grep if nothing found
	if #items == 0 then
		require("telescope.builtin").grep_string({ search = word })
		return
	end

	-- Sort using ranking logic
	table.sort(items, function(a, b)
		return score_entry(a) > score_entry(b)
	end)

	-- Launch picker
	create_dwim_picker(items)
end

return {
	dwim_definition = run,
}
