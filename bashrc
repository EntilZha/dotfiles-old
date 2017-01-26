SCALA_HOME=/usr/local/share/scala-2.11.0
EDITOR=mvim

export GOPATH=$HOME/Code/go
unset GOROOT

export PATH=$(/usr/local/bin/brew --prefix coreutils)/libexec/gnubin:$PATH
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH=/Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/Contents/Home/bin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=/usr/local/Cellar/macvim/7.4-77/bin:$PATH
export PATH=/Users/pedro/Utilities/activator-1.3.7-minimal:$PATH
export PATH=$PATH:/Users/pedro/Documents/Code/kenlm/bin
export PATH=$PATH:/Users/pedro/Utilities/termpdf
export PATH=/Users/pedro/Library/Android/sdk/platform-tools:$PATH
export PATH="$HOME/.node/bin:$PATH"
export PATH=$PATH:/Users/pedro/.npm-packages/bin
export PATH=$PATH:/Users/pedro/Utilities/bin
export PATH=/usr/local/bin:$PATH
export PATH=$PATH:/Library/TeX/Distributions/.DefaultTeX/Contents/Programs/texbin
export PATH="/Users/pedro/anaconda3/bin:$PATH"
export PATH=$PATH:/Users/pedro/.cargo/bin

export MYPYPATH=$MYPYPATH:~/Documents/Code/PyFunctional
export MYPYPATH=$MYPYPATH:~/anaconda3/lib/python3.5/site-packages
export MYPYPATH=$MYPYPATH:~/Utilities/spark-2.0.0-bin-hadoop2.7/python/

export GRAPPA_PREFIX=/Users/pedro/Code/grappa/build/Make+Release/install

export CC=clang
export CXX=clang++

export OPENSSL_LIB_DIR=/usr/local/Cellar/openssl/1.0.2f/lib/
export OPENSSL_INCLUDE_DIR=/usr/local/Cellar/openssl/1.0.2f/include/
export C_INCLUDE_PATH=/usr/local/Cellar/openssl/1.0.2f/include:$C_INCLUDE_PATH

export BOOST_ROOT=/Users/pedro/Utilities/boost_1_60_0

export PYTHONPATH=$PYTHONPATH:~/Code/pelican-plugins:/Users/pedro/Utilities/spark-1.6.1-bin-hadoop2.6/python
export PYTHONPATH=$PYTHONPATH:~/Code/qb

export PYSPARK_PYTHON=python3

export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/Contents/Home

source ~/.secrets
source ~/pinafore-openrc.sh
# us west-1 key, don't use for now
ssh-add ~/.ssh/odc-data.pem > /dev/null 2>&1
ssh-add ~/pedro-key.pem 2>&1

export QB_QUESTION_DB=/Users/pedro/Documents/Code/qb/data/internal/non_naqt.db
export QB_GUESS_DB=/Users/pedro/Documents/Code/qb/data/guesses.db
export QB_ROOT=/Users/pedro/Documents/Code/qb/
export QB_SPARK_MASTER="spark://terminus.local:7077"
export QB_AWS_S3_BUCKET="entilzha-us-west-2"
export QB_AWS_S3_NAMESPACE="pedro"
export QB_SECURITY_GROUPS=sg-61314c18

export TF_VAR_key_pair="pedro-key"
export TF_VAR_qb_aws_s3_bucket="entilzha-us-west-2"
export TF_VAR_qb_aws_s3_namespace="pedro"

export EC2_HOME=/Users/pedro/Utilities/ec2-api-tools-1.7.5.1

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
alias qbssh="ssh -i ~/Downloads/pedro-key.pem ubuntu@52.9.103.244"
alias terminus="ssh pedro@terminus.entilzha.io"
alias glances="glances -1"
alias gl="glances -1"
alias awssh=aws_ssh
alias s3jupyter="jupyter notebook --config ~/jupyter_s3_config.py"

aws_ssh() {
  ssh -i ~/Downloads/pedro-key.pem "ubuntu@$1"
}

tar_compress() {
	tar -zcvf $1.tar.gz $1
}

tar_decompress() {
	tar -zxvf $1
}

alias tarc=tar_compress
alias tard=tar_decompress

alias svim="sudo -E vim"

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

# added by travis gem
[ -f /Users/pedro/.travis/travis.sh ] && source /Users/pedro/.travis/travis.sh

# Configure man pages
export LESS_TERMCAP_mb=$(printf '\e[01;31m') # enter blinking mode - red
export LESS_TERMCAP_md=$(printf '\e[01;32m') # enter double-bright mode - bold, magenta
export LESS_TERMCAP_me=$(printf '\e[0m') # turn off all appearance modes (mb, md, so, us)
export LESS_TERMCAP_se=$(printf '\e[0m') # leave standout mode
export LESS_TERMCAP_so=$(printf '\e[01;31m') # enter standout mode - yellow
export LESS_TERMCAP_ue=$(printf '\e[0m') # leave underline mode
export LESS_TERMCAP_us=$(printf '\e[04;36m') # enter underline mode - cyan

