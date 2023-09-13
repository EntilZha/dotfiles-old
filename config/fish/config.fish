# GLOBALS
set -gx TERM xterm-256color
set -gx EDITOR vim
set -gx BAT_PAGER 'less -RF'
set -gx PAGER 'less'
set -gx LESS 'eFRX'
set -gx FZF_DEFAULT_COMMAND  'rg --files --hidden

'set -gx npm_config_prefix ~/.node_modules

fish_add_path ~/bin
fish_add_path ~/.local/bin
fish_add_path ~/go/bin
fish_add_path ~/.poetry/bin
fish_add_path ~/.node_modules/bin
fish_add_path ~/.cargo/bin
fish_add_path /Library/Tex/texbin/

fish_add_path ~/miniconda3/bin/

fish_ssh_agent


if test -e ~/.secrets.fish
    . ~/.secrets.fish
end

set fish_greeting

# Aliases
alias s3jupyter="jupyter notebook --config ~/jupyter_s3_config.py"
alias gl=glances
alias squeue="ssh df squeue"
alias gtop="watch --color -n1.0 gpustat -c"
alias l="exa"
alias tree="exa --tree"
alias b="bat"
alias colab="jupyter notebook --no-browser --NotebookApp.allow_origin='https://colab.research.google.com' --port=8888 --NotebookApp.port_retries=0"
alias c=conda
alias m=mamba
alias ca="c activate"
alias cenv="c activate (c env list | rg -v '^#' | rg '.+' | fzf | sd '(\S+)\b.+' '$1')"
alias rr=rip
alias g="git"
alias v="vim"
alias ncdu="ncdu --color dark"
alias icat="kitty +kitten icat"
alias d="kitty +kitten diff"
alias jl="jless"

alias fconf="vim ~/.config/fish/config.fish"
alias xconf="vim ~/.xmonad/xmonad.hs"

# Configure man pages
set -gx LESS_TERMCAP_mb (printf '\e[01;31m') # enter blinking mode - red
set -gx LESS_TERMCAP_md (printf '\e[01;32m') # enter double-bright mode - bold, magenta
set -gx LESS_TERMCAP_me (printf '\e[0m') # turn off all appearance modes (mb, md, so, us)
set -gx LESS_TERMCAP_se (printf '\e[0m') # leave standout mode
set -gx LESS_TERMCAP_so (printf '\e[01;31m') # enter standout mode - yellow
set -gx LESS_TERMCAP_ue (printf '\e[0m') # leave underline mode
set -gx LESS_TERMCAP_us (printf '\e[04;36m') # enter underline mode - cyan

function gdiff
  git diff (git log --pretty=oneline | fzf | sd '(a-z)* .*' '$1')
end

function pacs
  pacman -Q | rg $1
end

function custom_key_binds
  fish_vi_key_bindings

  bind -M insert \ca beginning-of-line
  bind -M insert \ce end-of-line
  bind -M insert \cf accept-autosuggestion
end
set -g fish_key_bindings custom_key_binds

# This sets up the conda command, but doesn't mangle path
source ~/miniconda3/etc/fish/conf.d/conda.fish

function jv
  jq -C . $argv | less -R
end

function qrurl
  qrencode -s 7 -o - $argv | icat
end

#atuin init fish | source
zoxide init fish | source
starship init fish | source
