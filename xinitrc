[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

xscreensaver -no-splash &
xbindkeys
gsettings get org.gnome.Terminal.Legacy.Settings confirm-close
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
autocutsel -selection CLIPBOARD -fork &

xsetroot -xcf /usr/share/icons/Breeze-Hacked/cursors/left_ptr 15
bash ~/.screenlayout/default.sh
feh --bg-max --randomize /screensaver/* &
trayer --edge top --align right --widthtype request --transparent true --tint 0x151a1c --alpha 0 --monitor primary --height 20 &
QT_SCALE_FACTOR=1 albert &
volumeicon &
flameshot &
