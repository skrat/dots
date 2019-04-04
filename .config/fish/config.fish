# Supress welcome message
set fish_greeting

# Path to Oh My Fish install.
set -gx OMF_PATH "/home/skrat/.local/share/omf"

# Tell OMF where z is
set -g Z_SCRIPT_PATH "/usr/lib/z.sh"

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

# FZF
set -U FZF_LEGACY_KEYBINDINGS 0
set -U FZF_DEFAULT_OPTS "--cycle --ansi --color=dark,bg+:-1,hl+:#edb443"
source $OMF_PATH/pkg/fzf/conf.d/fzf_key_bindings.fish

# Enable virtualenv (vf)
# eval (python -m virtualfish)

alias mosh="env LC_TIME= mosh"
alias gst="git status"
alias gdf="git difftool --dir-diff"
alias gcm="git commit -m"
alias e="emacsclient -n"
alias s="sudo"
alias rm="gio trash"

set -gx EDITOR vim
set -gx VISUAL vim
set -gx MAKEFLAGS "-j2"

# Clojure stuff
set -gx LEIN_FAST_TRAMPOLINE y
set -gx LEIN_JAVA_CMD /usr/bin/drip
set -gx BOOT_JAVA_COMMAND /usr/bin/drip

set -gx BAT_STYLE plain

# Behold The PATH
set -gx PATH $PATH $HOME/.workspace/bin $HOME/.local/bin

# OPAM configuration
. /home/skrat/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true
