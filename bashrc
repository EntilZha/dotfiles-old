SCALA_HOME=/usr/local/share/scala-2.11.0
EDITOR=mvim
SPARK_HOME=/Users/pedro/spark-0.9.0
PATH=$HOME/.rvm/bin:$PATH:$HOME/Utilities/Appify:/Applications/MATLAB_R2013a_Student.app/bin:"/Applications/Android Studio.app/sdk/platform-tools"
PATH=$PATH:$SCALA_HOME/bin:$HOME/Utilities
export GOPATH=$HOME/Code/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/opt/go/libexec/bin
function google() { open "https://www.google.com/search?q=$@"; }
alias hide_hidden="defaults write com.apple.finder AppleShowAllFiles FALSE;killall Finder"
alias show_hidden="defaults write com.apple.finder AppleShowAllFiles TRUE;killall Finder"
alias hopper="ssh -l pedro14 hopper.nersc.gov"
alias carver="ssh -l pedro14 carver.nersc.gov"
alias up="cd .."
alias home="cd ~"
alias h="cd ~"
alias hsearch="history | grep"
alias snowgeek="cd ~/Documents/Code/snowgeek/"
alias code="cd ~/Documents/Code"
alias py="ipython"
alias pyi="ipython -i"
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias known-rm="rm ~/.ssh/known_hosts"
alias gocode="cd $GOPATH/src/github.com/EntilZha"
alias avyvids="go run $GOPATH/src/github.com/EntilZha/local-fserver/main.go"
alias sudo="sudo -E"
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
