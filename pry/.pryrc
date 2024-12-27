if ENV['TERM'] == 'emacs'
  Pry.config.color = false
  Pry.config.pager = false
  Pry.config.auto_indent = false
end
