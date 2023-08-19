#!/usr/bin/env zsh

###############################
# EXPORT ENVIRONMENT VARIABLE #
###############################

# Man pages
export MANPAGER='nvim +Man!'

# fzf
export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

FZF_COLORS="bg+:-1,\
fg:gray,\
fg+:white,\
border:black,\
spinner:0,\
hl:yellow,\
header:blue,\
info:green,\
pointer:red,\
marker:blue,\
prompt:red,\
hl+:red"

export FZF_DEFAULT_OPTS="--height 60% \
--border rounded \
--layout reverse \
--color '$FZF_COLORS' \
--prompt '∷ ' \
--pointer ▶ \
--marker ⇒"

export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -n 10'"
export FZF_COMPLETION_DIR_COMMANDS="cd pushd rmdir --preview 'tree ls'"

export FZF_CTRL_R_OPTS="--preview 'echo {}' \
--preview-window up:3:hidden:wrap \
--bind 'ctrl-/:toggle-preview' \
--bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort' \
--color header:italic \--header 'Press CTRL-Y to copy command into clipboard'"

# Rust
. "$HOME/.cargo/env"
