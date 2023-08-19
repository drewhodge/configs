#!/usr/bin/env zsh

# List install files for dotfiles
# fdot() {
#     file=$(find "$DOTFILES/install" -exec basename {} ';' | sort | uniq | nl | fzf | cut -f 2)
#     [ -n "$file" ] && "$EDITOR" "$DOTFILES/install/$file"
# }

# List projects
# fwork() {
#     result=$(find ~/workspace/* -type d -prune -exec basename {} ';' | sort | uniq | nl | fzf | cut -f 2)
#     [ -n "$result" ] && cd ~/workspace/$result
# }

# Open pdf with Zathura
# fpdf() {
#     result=$(find -type f -name '*.pdf' | fzf --bind "ctrl-r:reload(find -type f -name '*.pdf')" --preview "pdftotext {} - | less")
#     [ -n "$result" ] && nohup zathura "$result" &> /dev/null & disown
# }

# Open epubs with Zathura
# fepub() {
#     result=$(find -type f -name '*.epub' | fzf --bind "ctrl-r:reload(find -type f -name '*.epub')")
#     [ -n "$result" ] && nohup zathura "$result" &> /dev/null & disown
# }

# Open freemind mindmap
# fmind() {
#     local folders=("$CLOUD/knowledge_base" "$WORKSPACE/alexandria")

#     files=""
#     for root in ${folders[@]}; do
#         files="$files $(find $root -name '*.mm')"
#     done
#     result=$(echo "$files" | fzf -m --height 60% --border sharp | tr -s "\n" " ")
#     [ -n "$result" ] && nohup freemind $(echo $result) &> /dev/null & disown
# }

# List tracking spreadsheets (productivity, money ...)
# ftrack() {
#     file=$(ls $CLOUD/tracking/**/*.{ods,csv} | fzf) || return
#     [ -n "$file" ] && libreoffice "$file" &> /dev/null &
# }

# Search and find directories in the dir stack
# fpop() {
#     # Only work with alias d defined as:
    
#     # alias d='dirs -v'
#     # for index ({1..9}) alias "$index"="cd +${index}"; unset index

#     d | fzf --height="20%" | cut -f 1 | source /dev/stdin
# }

# Find in File using ripgrep
fif() {
  if [ ! "$#" -gt 0 ]; then return 1; fi
  rg --files-with-matches --no-messages "$1" \
      | fzf --preview "highlight -O ansi -l {} 2> /dev/null \
      | rg --colors 'match:bg:yellow' --ignore-case --pretty --context 10 '$1' \
      || rg --ignore-case --pretty --context 10 '$1' {}"
}

# Find in file using ripgrep-all
fifa() {
    if [ ! "$#" -gt 0 ]; then return 1; fi
    local file
    file="$(rga --max-count=1 --ignore-case --files-with-matches --no-messages "$*" \
        | fzf-tmux -p +m --preview="rga --ignore-case --pretty --context 10 '"$*"' {}")" \
        && print -z "./$file" || return 1;
}

# Search through all man pages
function fman() {
    man -k . | fzf -q "$1" --prompt='man> '  --preview $'echo {} | tr -d \'()\' | awk \'{printf "%s ", $2} {print $1}\' | xargs -r man' | tr -d '()' | awk '{printf "%s ", $2} {print $1}' | xargs -r man
}
