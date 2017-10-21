EDITOR=mvim

function git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* (*\([^)]*\))*/\1/'
}

function markup_git_branch {
  if [[ -n $@ ]]; then
    if [[ -z $(git status --porcelain 2> /dev/null | tail -n1) ]]; then
      echo -e " \001\033[32m\002($@)\001\033[0m\002"
    else
      echo -e " \001\033[31m\002($@)\001\033[0m\002"
    fi
  fi
}

export PS1="\[\033[38;5;40m\]\u\[$(tput sgr0)\]\[\033[38;5;15m\] on \[$(tput sgr0)\]\[\033[38;5;9m\]\h\[$(tput sgr0)\]\[\033[38;5;15m\] at \[$(tput sgr0)\]\[\033[38;5;14m\]\A\[$(tput sgr0)\]\[\033[38;5;15m\] in \[$(tput sgr0)\]\[\033[38;5;12m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\]\$(markup_git_branch \$(git_branch))\n\\$ \[$(tput sgr0)\]"

export PATH=$(/usr/local/bin/brew --prefix coreutils)/libexec/gnubin:$PATH
export PATH="$HOME/.node/bin:$PATH"
export PATH=$PATH:/Users/pedro/.npm-packages/bin
export PATH=$PATH:/Users/pedro/Utilities/bin
export PATH=/usr/local/bin:$PATH
export PATH=$PATH:/Library/TeX/Distributions/.DefaultTeX/Contents/Programs/texbin
export PATH="/Users/pedro/anaconda3/bin:$PATH"
export PATH=$PATH:/Users/pedro/.cargo/bin

export SCALA_HOME=/usr/local/share/scala-2.11.0

export CC=clang
export CXX=clang++

export OPENSSL_LIB_DIR=/usr/local/Cellar/openssl/1.0.2f/lib/
export OPENSSL_INCLUDE_DIR=/usr/local/Cellar/openssl/1.0.2f/include/
export C_INCLUDE_PATH=/usr/local/Cellar/openssl/1.0.2f/include:$C_INCLUDE_PATH

export BOOST_ROOT=/Users/pedro/Utilities/boost_1_60_0

export PYTHONPATH=$PYTHONPATH:~/Code/pelican-plugins
export PYTHONPATH=$PYTHONPATH:~/Code/qb
export PYTHONPATH=$PYTHONPATH:/Users/pedro/Utilities/spark-2.1.1-bin-hadoop2.7/python

export PYSPARK_PYTHON=python3

export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/Contents/Home

source ~/.secrets
source ~/pinafore-openrc.sh
ssh-add ~/pedro-key.pem 2>/dev/null
ssh-add ~/eos_id_rsa 2>/dev/null

export QB_QUESTION_DB=/Users/pedro/Documents/Code/qb/data/internal/naqt.db
export QB_ROOT=/Users/pedro/Documents/Code/qb/
export QB_SPARK_MASTER="spark://terminus.local:7077"
export QB_AWS_S3_BUCKET="entilzha-us-west-2"
export QB_AWS_S3_NAMESPACE="journal-paper"
export QB_SECURITY_GROUPS=sg-feab8f86

export TF_VAR_key_pair="pedro-key"
export TF_VAR_qb_aws_s3_bucket="entilzha-us-west-2"
export TF_VAR_qb_aws_s3_namespace="journal-paper"

export TERM=xterm-256color

alias sudo="sudo -E"
alias skim="open -a Skim"
alias pycharm="/Applications/PyCharm\ CE.app/Contents/MacOS/pycharm"
alias publish-blog="pelican content -o output -s publishconf.py && ghp-import -b master -m 'Updated website' output"
alias s3jupyter="jupyter notebook --config ~/jupyter_s3_config.py"

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

