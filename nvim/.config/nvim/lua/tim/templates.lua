-- File templates for common file types
-- Uses shared templates from dotfiles/templates directory

-- Get the dotfiles templates directory
-- Resolve symlinks (for GNU Stow) to find actual config location
local config_path = vim.fn.stdpath("config")
local resolved_config = vim.fn.resolve(config_path)
-- From ~/dotfiles/nvim/.config/nvim, go up 3 levels to ~/dotfiles, then into templates/
local template_dir = vim.fn.fnamemodify(resolved_config, ":h:h:h") .. "/templates"

-- Helper function to load template
local function load_template(template_file)
	local template_path = template_dir .. "/" .. template_file
	if vim.fn.filereadable(template_path) == 1 then
		vim.cmd("0read " .. vim.fn.fnameescape(template_path))
		-- Delete the extra blank line at the end
		vim.cmd("$delete")
		-- Move cursor to a sensible position (usually line 1)
		vim.cmd("1")
	end
end

-- Simple default templates for each file type
-- For more complex file creation (classes, interfaces, etc.), use the `new-file` command
local templates = {
	{ pattern = "*.php", template = "php/default.php" },
	{ pattern = "*.vue", template = "vue/default.vue" },
	{ pattern = "*.html", template = "html/default.html" },
	{ pattern = "*.ts", template = "typescript/default.ts" },
	{ pattern = "*.js", template = "javascript/default.js" },
	{ pattern = "*.sh", template = "shell/default.sh" },
	{ pattern = "*.py", template = "python/default.py" },
}

for _, t in ipairs(templates) do
	vim.api.nvim_create_autocmd("BufNewFile", {
		pattern = t.pattern,
		callback = function()
			load_template(t.template)
		end,
	})
end
