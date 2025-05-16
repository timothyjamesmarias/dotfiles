return {
	"CopilotC-Nvim/CopilotChat.nvim",
	dependencies = {
		{ "github/copilot.vim" },
		{ "nvim-lua/plenary.nvim", branch = "master" },
	},
	build = "make tiktoken",
	opts = {
		window = {
			layout = "vertical",
			width = 0.25,
		},
	},
	keys = {
		{ "<Leader>cc", "<cmd>CopilotChat<cr>", desc = "Open Copilot Chat" },
		{ "<Leader>cn", "<cmd>CopilotChatNextMessage<cr>", desc = "Next Chat Message" },
		{ "<Leader>cp", "<cmd>CopilotChatPreviousMessage<cr>", desc = "Prev Chat Message" },
    { "<C-l>", false }, -- this disables any mapping for <C-l>
		{ "<Leader>cs", "<cmd>CopilotChatSendLine<cr>", desc = "Send Current Line" },
		{ "<leader>ch", "<cmd>Telescope copilot_chat history<cr>", desc = "View all copilot chats in telescope" },
	},
}
