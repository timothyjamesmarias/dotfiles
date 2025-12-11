vim.api.nvim_create_user_command("Ebuf", function(opts)
	local current_dir = vim.fn.expand("%:h")
	local file_path = current_dir .. "/" .. opts.args
	vim.cmd("edit " .. vim.fn.fnameescape(file_path))
end, {
	nargs = 1,
	complete = "file",
})

-- Ripgrep to quickfix command
vim.api.nvim_create_user_command("Rg", function(opts)
	local pattern = opts.args
	if pattern == "" then
		print("Usage: :Rg <pattern>")
		return
	end

	local cmd = string.format("rg --vimgrep --no-heading --hidden --glob='!.git' '%s'", pattern)
	local results = vim.fn.systemlist(cmd)

	if vim.v.shell_error ~= 0 or #results == 0 then
		print("No matches found for: " .. pattern)
		return
	end

	vim.fn.setqflist({}, "r", {
		title = "Rg: " .. pattern,
		lines = results,
		efm = "%f:%l:%c:%m",
	})
	vim.cmd("copen")
end, { nargs = 1 })

-- Ripgrep with type filters
vim.api.nvim_create_user_command("Rgjs", function(opts)
	vim.cmd("Rg " .. opts.args .. " --type js --type ts")
end, { nargs = 1 })

vim.api.nvim_create_user_command("Rgrb", function(opts)
	vim.cmd("Rg " .. opts.args .. " --type ruby")
end, { nargs = 1 })
