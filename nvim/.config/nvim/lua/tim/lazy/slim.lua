return {
	"slim-template/vim-slim",
	config = function()
		vim.cmd([[au BufNewFile,BufRead *.slim setlocal filetype=slim]])
	end,
}
