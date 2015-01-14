SCALA_HOME=/usr/local/share/scala-2.11.0
EDITOR=mvim
export PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH
export CLASSPATH=$CLASSPATH:~/Documents/Java-Packages/java-aws-mturk-1.6.2/lib

source ~/.secrets

function google() { open "https://www.google.com/search?q=$@"; }

alias hide_hidden="defaults write com.apple.finder AppleShowAllFiles FALSE;killall Finder"
alias show_hidden="defaults write com.apple.finder AppleShowAllFiles TRUE;killall Finder"
alias up="cd .."
alias home="cd ~"
alias h="cd ~"
alias hsearch="history | grep"
alias py="ipython"
alias pyi="ipython -i"
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias known-rm="rm ~/.ssh/known_hosts"
alias sudo="sudo -E"
alias skim="open -a Skim"

gui_vim() {
	if [[ $(uname) == 'Linux' ]]; then
		gvim "$@"
	else
		mvim "$@"
	fi
}

alias svim="sudo -E gui_vim"
alias vim="gui_vim"

markdown() {
	file=$1
	filename=$(basename "${file%%.*}")
	pdf=".pdf"
	md=".md"
	md_filename=$filename$md
	pdf_filename=$filename$pdf
	vim $md_filename
	open -a Skim $pdf_filename
}
