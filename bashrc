SCALA_HOME=/usr/local/share/scala-2.11.0
EDITOR=mvim

export GOPATH=$HOME/Code/go
unset GOROOT

export PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH=/Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/Contents/Home/bin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=/usr/local/Cellar/macvim/7.4-77/bin:$PATH
export PATH=/Users/pedro/Utilities/activator-dist-1.3.6:$PATH
export PATH=/Users/pedro/.cargo/bin:$PATH
export PATH=$PATH:/Users/pedro/Documents/Code/kenlm/bin

export GRAPPA_PREFIX=/Users/pedro/Code/grappa/build/Make+Release/install

export CC=/usr/local/Cellar/gcc/5.3.0/bin/gcc-5
export CXX=/usr/local/Cellar/gcc/5.3.0/bin/g++-5

export RUST_SRC_PATH=/Users/pedro/Documents/Code/rust/src/

export BOOST_ROOT=/Users/pedro/Utilities/boost_1_60_0

export PYTHONPATH=$PYTHONPATH:~/Code/pelican-plugins
export PYTHONPATH=$PYTHONPATH:~/Utilities/spark-1.5.0/python
export CLASSPATH=$CLASSPATH:~/Documents/Java-Packages/java-aws-mturk-1.6.2/lib:~/Documents/Java-Packages/java-aws-mturk-1.6.2/lib/third-party

export SPARK_HOME=~/Utilities/spark-1.5.0
export PYSPARK_PYTHON=python3

export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/Contents/Home

source ~/.secrets
source ~/pinafore-openrc.sh
export DOCKER_TLS_VERIFY="1"
export DOCKER_HOST="tcp://192.168.99.100:2376"
export DOCKER_CERT_PATH="/Users/pedro/.docker/machine/machines/dev"
export DOCKER_MACHINE_NAME="dev"

export QB_QUESTION_DB=/Users/pedro/Documents/Code/qb/data/non_naqt.db
export QB_GUESS_DB=/Users/pedro/Documents/Code/qb/data/guesses.db
export QB_ROOT=/Users/pedro/Documents/Code/qb/
export QB_SPARK_MASTER="spark://terminus.local:7077"

export TERM=xterm-256color

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
alias publish-blog="pelican content -o output -s publishconf.py && ghp-import -b master -m 'Updated website' output"

tar_compress() {
	tar -zcvf $1.tar.gz $1
}

tar_decompress() {
	tar -zxvf $1
}

alias tarc=tar_compress
alias tard=tar_decompress

gui_vim() {
	if [[ $(uname) == 'Linux' ]]; then
		gvim "$@"
	else
		mvim "$@"
	fi
}

alias svim="sudo -E gui_vim"
alias mvim="gui_vim"

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

docker_rmc() {
	docker rm $(docker ps -a -q)
}
