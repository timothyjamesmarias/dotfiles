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

-- Telescope keymaps
function vim.getVisualSelection()
	local _, ls_row, ls_col, _ = unpack(vim.fn.getpos("v"))
	local _, le_row, le_col, _ = unpack(vim.fn.getpos("."))
	if ls_row > le_row or (ls_row == le_row and ls_col > le_col) then
		ls_row, le_row = le_row, ls_row
		ls_col, le_col = le_col, ls_col
	end
	local lines = vim.fn.getline(ls_row, le_row)
	if #lines == 0 then
		return ""
	end
	lines[1] = string.sub(lines[1], ls_col)
	lines[#lines] = string.sub(lines[#lines], 1, le_col)
	return table.concat(lines, " ")
end

vim.keymap.set("n", "<leader>fs", "<cmd>Telescope find_files follow=true hidden=true<CR>", { silent = true })
vim.keymap.set("v", "<leader>fw", function()
	local text = vim.getVisualSelection()
	require("telescope.builtin").live_grep({ default_text = text })
end)
vim.keymap.set("v", "<leader>fs", function()
	local text = vim.getVisualSelection()
	require("telescope.builtin").find_files({ default_text = text })
end, { silent = true, noremap = true })
vim.keymap.set("n", "<leader>fr", function() require("telescope.builtin").oldfiles() end, { silent = true })
vim.keymap.set("n", "<leader>fg", function() require("telescope.builtin").git_commits() end, { silent = true })
vim.keymap.set("n", "<leader>fw", function() require("telescope.builtin").live_grep() end, { silent = true })
vim.keymap.set("n", "<leader>fk", function() require("telescope.builtin").keymaps() end, { silent = true })
vim.keymap.set("n", "<leader>fcc", function() require("telescope.builtin").commands() end, { silent = true })
vim.keymap.set("n", "<leader>fca", function() require("telescope.builtin").autocommands() end, { silent = true })
vim.keymap.set("n", "<leader>fj", function() require("telescope.builtin").jumplist() end, { silent = true })
vim.keymap.set("n", "<leader>fb", function() require("telescope.builtin").buffers() end, { silent = true })
vim.keymap.set("n", "<leader>fh", function() require("telescope.builtin").help_tags() end, { silent = true })
vim.keymap.set("n", "<leader>sp", function() require("telescope.builtin").spell_suggest() end, { silent = true })
vim.keymap.set("n", "<leader>fn", function()
	require("telescope").extensions.file_browser.file_browser({
		path = "%:p:h",
		select_buffer = true,
		grouped = true,
		hidden = true,
		previewer = false,
		layout_config = { height = 0.5 },
	})
end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fp", function()
	local git_root = vim.fn.systemlist("git rev-parse --show-toplevel")[1]
	require("telescope").extensions.file_browser.file_browser({
		path = git_root,
		grouped = true,
		hidden = true,
		previewer = false,
		layout_config = { height = 0.5 },
	})
end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>ts", function() require("telescope.builtin").tagstack() end, { silent = true })
vim.keymap.set("n", "<leader>tg", function() require("telescope.builtin").tags() end, { silent = true })
vim.keymap.set("n", "<leader>sl", function() require("telescope.builtin").grep_string() end, { silent = true, noremap = true })
vim.keymap.set("v", "<leader>sl", function()
	local text = vim.getVisualSelection()
	require("telescope.builtin").grep_string({ search = text })
end, { silent = true, noremap = true })

-- LSP keymaps (set on LSP attach)
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local bufnr = args.buf
		local opts = { buffer = bufnr, silent = true, noremap = true }
		local telescope_builtin = require("telescope.builtin")

		-- Navigation (using Telescope for better UX)
		vim.keymap.set("n", "<leader>ld", telescope_builtin.lsp_definitions, opts)
		vim.keymap.set("n", "<leader>lD", vim.lsp.buf.declaration, opts)
		vim.keymap.set("n", "<leader>li", telescope_builtin.lsp_implementations, opts)
		vim.keymap.set("n", "<leader>lt", telescope_builtin.lsp_type_definitions, opts)
		vim.keymap.set("n", "<leader>lr", telescope_builtin.lsp_references, opts)

		-- Documentation
		vim.keymap.set("n", "<leader>lh", vim.lsp.buf.hover, opts)
		vim.keymap.set("n", "<leader>ls", vim.lsp.buf.signature_help, opts)

		-- Code actions
		vim.keymap.set("n", "<leader>la", vim.lsp.buf.code_action, opts)
		vim.keymap.set("n", "<leader>ln", vim.lsp.buf.rename, opts)
		-- Use formatter.nvim instead of LSP formatting
		vim.keymap.set("n", "<leader>lf", "<cmd>Format<CR>", opts)

		-- Diagnostics (using Telescope for diagnostics lists)
		vim.keymap.set("n", "<leader>le", vim.diagnostic.open_float, opts)
		vim.keymap.set("n", "<leader>lq", telescope_builtin.diagnostics, opts)
		vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
		vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)

		-- Workspace
		vim.keymap.set("n", "<leader>lwa", vim.lsp.buf.add_workspace_folder, opts)
		vim.keymap.set("n", "<leader>lwr", vim.lsp.buf.remove_workspace_folder, opts)
		vim.keymap.set("n", "<leader>lwl", function()
			print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
		end, opts)

		-- Document symbols (using Telescope)
		vim.keymap.set("n", "<leader>lo", telescope_builtin.lsp_document_symbols, opts)
		vim.keymap.set("n", "<leader>lO", telescope_builtin.lsp_workspace_symbols, opts)

		-- Incoming/Outgoing calls
		vim.keymap.set("n", "<leader>lci", telescope_builtin.lsp_incoming_calls, opts)
		vim.keymap.set("n", "<leader>lco", telescope_builtin.lsp_outgoing_calls, opts)

		-- LSP control
		vim.keymap.set("n", "<leader>lR", "<cmd>LspRestart<CR>", opts)
		vim.keymap.set("n", "<leader>lI", "<cmd>LspInfo<CR>", opts)
	end,
})

-- Kotlin/Java Gradle keymaps (set on filetype)
vim.api.nvim_create_autocmd("FileType", {
	pattern = { "kotlin", "java" },
	callback = function()
		local opts = { buffer = true, silent = true, noremap = true }

		-- Gradle integration keymaps
		vim.keymap.set("n", "<leader>gb", ":!./gradlew build<CR>", opts)
		vim.keymap.set("n", "<leader>gt", ":!./gradlew test<CR>", opts)
		vim.keymap.set("n", "<leader>gr", ":!./gradlew run<CR>", opts)
		vim.keymap.set("n", "<leader>gc", ":!./gradlew clean<CR>", opts)
		vim.keymap.set("n", "<leader>gd", ":!./gradlew bootRun<CR>", opts)

		-- Quick compile check (no tests)
		vim.keymap.set("n", "<leader>gq", ":!./gradlew build -x test<CR>", opts)

		-- Run specific test under cursor (requires custom function)
		vim.keymap.set("n", "<leader>gT", function()
			local class_name = vim.fn.expand("%:t:r")
			vim.cmd("!./gradlew test --tests " .. class_name)
		end, opts)
	end,
})
