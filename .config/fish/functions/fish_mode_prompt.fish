function fish_mode_prompt --description 'Displays the current mode'
	# Do nothing if not in vi mode
  if test "$fish_key_bindings" = "fish_vi_key_bindings"
    switch $fish_bind_mode
      case default
        set_color --bold magenta
        echo 'N'
      case insert
        set_color --bold green
        echo 'I'
      case replace-one
        set_color --bold red
        echo 'R'
      case visual
        set_color --bold cyan
        echo 'V'
    end
    set_color normal
  end
end
