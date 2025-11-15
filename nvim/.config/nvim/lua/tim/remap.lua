-- ============================================================================
-- Keymap Organization
-- ============================================================================
-- <leader>w/q/Q     - Basic file operations (save, close, quit)
-- <leader>h/v       - Window splits
-- <leader>e*        - Edit/open files
-- <leader>s*        - Search operations
-- <leader>f*        - Find (Telescope) - file finding, grep, git, etc.
-- <leader>t*        - Tags (ctags navigation)
-- <leader>b*        - Buffer operations (navigation, switching)
-- <leader>l*        - LSP operations (definitions, references, diagnostics, etc.)
-- <leader>p*        - Project/Framework commands (context-aware based on filetype)
--                     * Rails: alternate, related, model, view, controller, generate, test, console
--                     * Laravel: artisan, routes, related/model
--                     * Gradle/Kotlin: build, test, run, clean, dev server
-- <leader>g*        - Git operations (via gitsigns or other git plugins)
-- <leader>x*        - Quickfix/Location list operations
-- <leader>cm        - Run shell command
-- <C-n/p>           - Buffer navigation (next/previous)
-- n/N               - Search navigation (centered)
-- ;                 - Command mode
-- ============================================================================

vim.g.mapleader = " "
vim.keymap.set("n", "<leader>w", "<cmd>w<CR>", { desc = "Save file" })
vim.keymap.set("n", ";", ":", { desc = "Enter command mode" })
vim.keymap.set(
	"n",
	"<leader>q",
	"<cmd>bp<bar>sp<bar>bn<bar>bd<CR>",
	{ silent = true, desc = "Close buffer without closing window" }
)
vim.keymap.set("n", "<leader>Q", "<cmd>q!<CR>", { silent = true, desc = "Force quit without saving" })
vim.keymap.set("n", "n", "nzzzv", { silent = true, desc = "Next search result (centered)" })
vim.keymap.set("n", "N", "Nzzzv", { silent = true, desc = "Previous search result (centered)" })
vim.keymap.set("n", "<leader>hh", "<cmd>vsp<CR>", { silent = true, desc = "Vertical split" })
vim.keymap.set("n", "<leader>vv", "<cmd>sp<CR>", { silent = true, desc = "Horizontal split" })
vim.keymap.set("n", "<leader>sf", "/", { desc = "Search in file" })
vim.keymap.set("i", "<C-e>", "<C-o>A", { desc = "Jump to end of line (insert mode)" })
vim.keymap.set("n", "<leader>cm", ":!", { desc = "Run shell command" })
vim.keymap.set("n", "<leader>ee", ":e ", { desc = "Edit file" })
vim.keymap.set("n", "<leader>eb", ":Ebuf ", { desc = "Edit buffer" })
vim.keymap.set("i", "<C-l>", "<C-o>l", { desc = "Move right one character (insert mode)" })
vim.keymap.set("n", "<leader>z", "<cmd>ZenMode<CR>", { silent = true, desc = "Toggle Zen Mode" })

-- navigation
vim.keymap.set("n", "<C-n>", "<cmd>bnext<CR>", { silent = true, remap = true, desc = "Next buffer" })
vim.keymap.set("n", "<C-p>", "<cmd>bprevious<CR>", { silent = true, remap = true, desc = "Previous buffer" })
vim.keymap.set("n", "<leader>ls", "<cmd>ls<CR>", { silent = true, remap = true, desc = "List buffers" })

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

-- Telescope and file keymaps
vim.keymap.set("n", "<leader>fo", "<cmd>!open -R %<CR>", { silent = true, desc = "Open current file in finder" })
vim.keymap.set("n", "<leader>fO", "<cmd>!open %<CR>", { silent = true, desc = "Open current file with the default program" })
vim.keymap.set("n", "<leader>fy", function()
	local path = vim.fn.expand("%:p")
	vim.fn.setreg("+", path)
	print("Copied to clipboard: " .. path)
end, { silent = false, desc = "Copy absolute file path to clipboard" })
vim.keymap.set("n", "<leader>fY", function()
	local path = vim.fn.expand("%:.")
	vim.fn.setreg("+", path)
	print("Copied to clipboard: " .. path)
end, { silent = false, desc = "Copy relative file path to clipboard" })

function vim.getVisualSelection()
	local _, ls_row, ls_col, _ = unpack(vim.fn.getpos("'<"))
	local _, le_row, le_col, _ = unpack(vim.fn.getpos("'>"))
	local lines = vim.fn.getline(ls_row, le_row)
	if #lines == 0 then
		return ""
	end

	-- Handle single-line selection separately to avoid double-modification
	if #lines == 1 then
		lines[1] = string.sub(lines[1], ls_col, le_col)
	else
		lines[1] = string.sub(lines[1], ls_col)
		lines[#lines] = string.sub(lines[#lines], 1, le_col)
	end

	return table.concat(lines, " ")
end

vim.keymap.set(
	"n",
	"<leader>fs",
	"<cmd>Telescope find_files follow=true hidden=true<CR>",
	{ silent = true, desc = "Find files" }
)
vim.keymap.set("v", "<leader>fw", function()
	local text = vim.getVisualSelection()
	require("telescope.builtin").live_grep({ default_text = text })
end, { desc = "Live grep with selection" })
vim.keymap.set("v", "<leader>fs", function()
	local text = vim.getVisualSelection()
	require("telescope.builtin").find_files({ default_text = text })
end, { silent = true, noremap = true, desc = "Find files with selection" })
vim.keymap.set("n", "<leader>fr", function()
	require("telescope.builtin").oldfiles()
end, { silent = true, desc = "Recent files" })
vim.keymap.set("n", "<leader>fg", function()
	require("telescope.builtin").git_commits()
end, { silent = true, desc = "Git commits" })
vim.keymap.set("n", "<leader>fw", function()
	require("telescope.builtin").live_grep()
end, { silent = true, desc = "Live grep" })
vim.keymap.set("n", "<leader>fk", function()
	require("telescope.builtin").keymaps()
end, { silent = true, desc = "Find keymaps" })
vim.keymap.set("n", "<leader>fcc", function()
	require("telescope.builtin").commands()
end, { silent = true, desc = "Find commands" })
vim.keymap.set("n", "<leader>fca", function()
	require("telescope.builtin").autocommands()
end, { silent = true, desc = "Find autocommands" })
vim.keymap.set("n", "<leader>fj", function()
	require("telescope.builtin").jumplist()
end, { silent = true, desc = "Jumplist" })
vim.keymap.set("n", "<leader>fb", function()
	require("telescope.builtin").buffers()
end, { silent = true, desc = "Find buffers" })
vim.keymap.set("n", "<leader>fh", function()
	require("telescope.builtin").help_tags()
end, { silent = true, desc = "Help tags" })
vim.keymap.set("n", "<leader>sp", function()
	require("telescope.builtin").spell_suggest()
end, { silent = true, desc = "Spell suggestions" })
vim.keymap.set("n", "<leader>fn", function()
	require("telescope").extensions.file_browser.file_browser({
		path = "%:p:h",
		select_buffer = true,
		grouped = true,
		hidden = true,
		previewer = false,
		layout_config = { height = 0.5 },
	})
end, { noremap = true, silent = true, desc = "File browser (current dir)" })
vim.keymap.set("n", "<leader>fp", function()
	local git_root = vim.fn.systemlist("git rev-parse --show-toplevel")[1]
	require("telescope").extensions.file_browser.file_browser({
		path = git_root,
		grouped = true,
		hidden = true,
		previewer = false,
		layout_config = { height = 0.5 },
	})
end, { noremap = true, silent = true, desc = "File browser (project root)" })
vim.keymap.set("n", "<leader>ts", function()
	require("telescope.builtin").tagstack()
end, { silent = true, desc = "Tag stack" })
vim.keymap.set("n", "<leader>tg", function()
	require("telescope.builtin").tags()
end, { silent = true, desc = "Find tags" })
vim.keymap.set("n", "<leader>sl", function()
	require("telescope.builtin").grep_string()
end, { silent = true, noremap = true, desc = "Grep string under cursor" })
vim.keymap.set("v", "<leader>sl", function()
	local text = vim.getVisualSelection()
	require("telescope.builtin").grep_string({ search = text })
end, { silent = true, noremap = true, desc = "Grep visual selection" })

-- LSP keymaps (set on LSP attach)
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local bufnr = args.buf
		local opts = { buffer = bufnr, silent = true, noremap = true }
		local telescope_builtin = require("telescope.builtin")

		-- Navigation (using Telescope for better UX)
		vim.keymap.set(
			"n",
			"<leader>ld",
			telescope_builtin.lsp_definitions,
			vim.tbl_extend("force", opts, { desc = "Go to definition" })
		)
		vim.keymap.set(
			"n",
			"<leader>lD",
			vim.lsp.buf.declaration,
			vim.tbl_extend("force", opts, { desc = "Go to declaration" })
		)
		vim.keymap.set(
			"n",
			"<leader>li",
			telescope_builtin.lsp_implementations,
			vim.tbl_extend("force", opts, { desc = "Go to implementation" })
		)
		vim.keymap.set(
			"n",
			"<leader>lt",
			telescope_builtin.lsp_type_definitions,
			vim.tbl_extend("force", opts, { desc = "Go to type definition" })
		)
		vim.keymap.set(
			"n",
			"<leader>lr",
			telescope_builtin.lsp_references,
			vim.tbl_extend("force", opts, { desc = "Find references" })
		)

		-- Documentation
		vim.keymap.set(
			"n",
			"<leader>lh",
			vim.lsp.buf.hover,
			vim.tbl_extend("force", opts, { desc = "Hover documentation" })
		)
		vim.keymap.set(
			"n",
			"<leader>ls",
			vim.lsp.buf.signature_help,
			vim.tbl_extend("force", opts, { desc = "Signature help" })
		)

		-- Code actions
		vim.keymap.set(
			"n",
			"<leader>la",
			vim.lsp.buf.code_action,
			vim.tbl_extend("force", opts, { desc = "Code action" })
		)
		vim.keymap.set("n", "<leader>ln", vim.lsp.buf.rename, vim.tbl_extend("force", opts, { desc = "Rename symbol" }))
		-- Use formatter.nvim instead of LSP formatting
		vim.keymap.set("n", "<leader>lf", "<cmd>Format<CR>", vim.tbl_extend("force", opts, { desc = "Format buffer" }))

		-- Diagnostics (using Telescope for diagnostics lists)
		vim.keymap.set(
			"n",
			"<leader>le",
			vim.diagnostic.open_float,
			vim.tbl_extend("force", opts, { desc = "Show diagnostic" })
		)
		vim.keymap.set(
			"n",
			"<leader>lq",
			telescope_builtin.diagnostics,
			vim.tbl_extend("force", opts, { desc = "List diagnostics" })
		)
		vim.keymap.set(
			"n",
			"[d",
			vim.diagnostic.goto_prev,
			vim.tbl_extend("force", opts, { desc = "Previous diagnostic" })
		)
		vim.keymap.set("n", "]d", vim.diagnostic.goto_next, vim.tbl_extend("force", opts, { desc = "Next diagnostic" }))

		-- Workspace
		vim.keymap.set(
			"n",
			"<leader>lwa",
			vim.lsp.buf.add_workspace_folder,
			vim.tbl_extend("force", opts, { desc = "Add workspace folder" })
		)
		vim.keymap.set(
			"n",
			"<leader>lwr",
			vim.lsp.buf.remove_workspace_folder,
			vim.tbl_extend("force", opts, { desc = "Remove workspace folder" })
		)
		vim.keymap.set("n", "<leader>lwl", function()
			print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
		end, vim.tbl_extend("force", opts, { desc = "List workspace folders" }))

		-- Document symbols (using Telescope)
		vim.keymap.set(
			"n",
			"<leader>lo",
			telescope_builtin.lsp_document_symbols,
			vim.tbl_extend("force", opts, { desc = "Document symbols" })
		)
		vim.keymap.set(
			"n",
			"<leader>lO",
			telescope_builtin.lsp_workspace_symbols,
			vim.tbl_extend("force", opts, { desc = "Workspace symbols" })
		)

		-- Incoming/Outgoing calls
		vim.keymap.set(
			"n",
			"<leader>lci",
			telescope_builtin.lsp_incoming_calls,
			vim.tbl_extend("force", opts, { desc = "Incoming calls" })
		)
		vim.keymap.set(
			"n",
			"<leader>lco",
			telescope_builtin.lsp_outgoing_calls,
			vim.tbl_extend("force", opts, { desc = "Outgoing calls" })
		)

		-- LSP control
		vim.keymap.set(
			"n",
			"<leader>lR",
			"<cmd>LspRestart<CR>",
			vim.tbl_extend("force", opts, { desc = "Restart LSP" })
		)
		vim.keymap.set("n", "<leader>lI", "<cmd>LspInfo<CR>", vim.tbl_extend("force", opts, { desc = "LSP info" }))
	end,
})


vim.keymap.set("n", "<leader>po", "<cmd>!open .<CR>", { silent = true, desc = "Open project directory in finder" })

-- Quickfix list navigation
vim.keymap.set("n", "]q", "<cmd>cnext<CR>zz", { desc = "Next quickfix item" })
vim.keymap.set("n", "[q", "<cmd>cprev<CR>zz", { desc = "Previous quickfix item" })
vim.keymap.set("n", "<leader>xo", "<cmd>copen<CR>", { desc = "Open quickfix list" })
vim.keymap.set("n", "<leader>xc", "<cmd>cclose<CR>", { desc = "Close quickfix list" })
vim.keymap.set("n", "<leader>xx", "<cmd>cexpr []<CR>", { desc = "Clear quickfix list" })
vim.keymap.set("n", "<leader>xr", ":Rg ", { desc = "Ripgrep to quickfix" })

-- Send current search pattern to quickfix
vim.keymap.set("n", "<leader>xs", function()
	local pattern = vim.fn.getreg("/")
	if pattern == "" then
		print("No search pattern set")
		return
	end
	-- Remove leading/trailing slashes from pattern
	pattern = pattern:gsub("^/", ""):gsub("/$", "")
	vim.cmd("vimgrep /" .. pattern .. "/gj **/*")
	vim.cmd("copen")
end, { desc = "Send current search to quickfix" })

-- Location list navigation (per-window quickfix)
vim.keymap.set("n", "]l", "<cmd>lnext<CR>zz", { desc = "Next location item" })
vim.keymap.set("n", "[l", "<cmd>lprev<CR>zz", { desc = "Previous location item" })
vim.keymap.set("n", "<leader>xl", "<cmd>lopen<CR>", { desc = "Open location list" })
vim.keymap.set("n", "<leader>xL", "<cmd>lclose<CR>", { desc = "Close location list" })

-- Ruby/Rails Project keymaps (set on filetype)
vim.api.nvim_create_autocmd("FileType", {
	pattern = { "ruby", "eruby", "slim" },
	callback = function()
		local opts = { buffer = true, silent = true, noremap = true }

		-- Project commands (Rails)
		vim.keymap.set("n", "<leader>pa", ":A<CR>", vim.tbl_extend("force", opts, { desc = "Project: Alternate file" }))
		vim.keymap.set("n", "<leader>pr", ":R<CR>", vim.tbl_extend("force", opts, { desc = "Project: Related file" }))
		vim.keymap.set("n", "<leader>pm", ":Emodel<CR>", vim.tbl_extend("force", opts, { desc = "Project: Model" }))
		vim.keymap.set("n", "<leader>pv", ":Eview<CR>", vim.tbl_extend("force", opts, { desc = "Project: View" }))
		vim.keymap.set(
			"n",
			"<leader>pc",
			":Econtroller<CR>",
			vim.tbl_extend("force", opts, { desc = "Project: Controller" })
		)
		vim.keymap.set("n", "<leader>pg", ":Generate ", vim.tbl_extend("force", opts, { desc = "Project: Generate" }))
		vim.keymap.set("n", "<leader>pt", ":Rails<CR>", vim.tbl_extend("force", opts, { desc = "Project: Test/Run" }))
		vim.keymap.set(
			"n",
			"<leader>pT",
			":.Rails<CR>",
			vim.tbl_extend("force", opts, { desc = "Project: Test current" })
		)
		vim.keymap.set(
			"n",
			"<leader>ps",
			":Rails console<CR>",
			vim.tbl_extend("force", opts, { desc = "Project: Console (Rails)" })
		)
	end,
})

-- ERB tag mappings (eruby files only)
vim.api.nvim_create_autocmd("FileType", {
	pattern = "eruby",
	callback = function()
		local opts = { buffer = true, silent = true, noremap = true }

		-- Insert ERB tags (normal mode)
		vim.keymap.set(
			"n",
			"<leader>ie",
			"i<%=  %><Esc>hhi",
			vim.tbl_extend("force", opts, { desc = "Insert ERB output tag" })
		)
		vim.keymap.set(
			"n",
			"<leader>im",
			"i<%  %><Esc>hhi",
			vim.tbl_extend("force", opts, { desc = "Insert ERB execution tag" })
		)
		vim.keymap.set(
			"n",
			"<leader>ic",
			"i<%#  %><Esc>hhi",
			vim.tbl_extend("force", opts, { desc = "Insert ERB comment tag" })
		)
	end,
})

