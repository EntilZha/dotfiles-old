function fish_prompt
  set_color green
  echo -n (whoami)
  set_color white
  echo -n " on "
  set_color red
  echo -n (hostname)
  set_color white
  echo -n " at "
  set_color purple
  echo -n (date "+%H:%M")
  set_color white
  echo -n " in "
  set_color cyan
  echo (pwd)
  set_color yellow
  echo -n "\$ "
  set_color normal
end

# GLOBALS
set -gx TERM xterm-256color
set -gx EDITOR vim

# Aliases
alias s3jupyter="jupyter notebook --config ~/jupyter_s3_config.py"
alias gl=glances
alias gtop="watch --color -n1.0 gpustat -c"
alias xclip="xclip -selection clipboard"
alias l="exa"
alias b="bat"
alias ncdu="ncdu --color dark"

# Configure man pages
set -gx LESS_TERMCAP_mb (printf '\e[01;31m') # enter blinking mode - red
set -gx LESS_TERMCAP_md (printf '\e[01;32m') # enter double-bright mode - bold, magenta
set -gx LESS_TERMCAP_me (printf '\e[0m') # turn off all appearance modes (mb, md, so, us)
set -gx LESS_TERMCAP_se (printf '\e[0m') # leave standout mode
set -gx LESS_TERMCAP_so (printf '\e[01;31m') # enter standout mode - yellow
set -gx LESS_TERMCAP_ue (printf '\e[0m') # leave underline mode
set -gx LESS_TERMCAP_us (printf '\e[04;36m') # enter underline mode - cyan

function pycharm
  wmname LG3D; and ~/applications/pycharm/bin/pycharm.sh &;
end

