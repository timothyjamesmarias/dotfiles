# Enable TUI automatically
set pagination off
set confirm off
set history save on
set history filename ~/.gdb_history

# Set a better prompt
set prompt (gdb) 

# Pretty printing
set print pretty on
set print elements 0

# Useful aliases
define b
  break $arg0
end

define r
  run
end

define c
  continue
end

define n
  next
end

define s
  step
end

define bt
  backtrace
end

# Better disassembly view
set disassembly-flavor intel

# Dashboard (if installed)
source ~/.gdb-dashboard
