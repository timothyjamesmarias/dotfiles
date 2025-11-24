-- OpenInNeovim.applescript
-- Opens files in Neovim via Alacritty and tmux
-- Compiles to .app with: osacompile -o ~/Applications/OpenInNeovim.app OpenInNeovim.applescript

on open theFiles
    repeat with theFile in theFiles
        set filePath to POSIX path of theFile
        openInNeovim(filePath)
    end repeat
end open

on run
    -- When launched without files, just open a new nvim instance
    openInNeovim("")
end run

on openInNeovim(filePath)
    set tmuxSession to "editor"

    if filePath is "" then
        set nvimCmd to "nvim"
    else
        -- Escape single quotes in file path
        set escapedPath to do shell script "printf '%s' " & quoted form of filePath & " | sed \"s/'/'\\\\''/g\""
        set nvimCmd to "nvim '" & escapedPath & "'"
    end if

    -- Check if tmux session exists
    try
        do shell script "/opt/homebrew/bin/tmux has-session -t " & tmuxSession & " 2>/dev/null"
        set sessionExists to true
    on error
        set sessionExists to false
    end try

    if sessionExists then
        -- Session exists: send keys to open file in existing nvim or create new window
        set shellCmd to "/opt/homebrew/bin/tmux new-window -t " & tmuxSession & " '" & nvimCmd & "'"
    else
        -- No session: create new session with nvim
        set shellCmd to "/opt/homebrew/bin/tmux new-session -d -s " & tmuxSession & " '" & nvimCmd & "'"
    end if

    do shell script shellCmd

    -- Open Alacritty and attach to the session
    tell application "Alacritty" to activate
    delay 0.2

    -- Attach to tmux session if not already attached
    tell application "System Events"
        tell process "Alacritty"
            keystroke "tmux attach -t " & tmuxSession & return
        end tell
    end tell
end openInNeovim
