vim.api.nvim_create_user_command("Ebuf", function(opts)
	local current_dir = vim.fn.expand("%:h")
	local file_path = current_dir .. "/" .. opts.args
	vim.cmd("edit" .. vim.fn.fnameescape(file_path))
end, {
	nargs = 1,
	complete = "file",
})
