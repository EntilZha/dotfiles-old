SCALA_HOME=/usr/local/share/scala-2.11.0
EDITOR=mvim

export GOPATH=$HOME/Code/go
unset GOROOT
export PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH=/Library/Java/JavaVirtualMachines/jdk1.7.0_79.jdk/Contents/Home/bin:$PATH
export PYTHONPATH=$PYTHONPATH:~/Code/datascience-crowdsourcing/python
export CLASSPATH=$CLASSPATH:~/Documents/Java-Packages/java-aws-mturk-1.6.2/lib:~/Documents/Java-Packages/java-aws-mturk-1.6.2/lib/third-party

export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=$HOME/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=1

export SPARK_HOME=$HOME/Code/spark
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_79.jdk/Contents/Home/

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
alias pycharm="/Applications/PyCharm\ CE.app/Contents/MacOS/pycharm"
alias gatling="~/Code/gatling-charts-highcharts-bundle-2.1.5/bin/gatling.sh"

alias ds1dev="ssh prodriguez@datascience1dev.sv2.trulia.com"

gui_vim() {
	if [[ $(uname) == 'Linux' ]]; then
		gvim "$@"
	else
		mvim "$@"
	fi
}

alias svim="sudo -E gui_vim"
alias tvim="gui_vim"
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
export PATH=/usr/local/sbin:$PATH
