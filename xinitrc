[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

xscreensaver -no-splash &
xbindkeys
feh --bg-max --randomize ~/screensaver/* &
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
autocutsel -selection CLIPBOARD -fork &
