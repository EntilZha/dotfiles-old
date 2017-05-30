# PATH
set -gx PATH /usr/local/bin $PATH
set -gx PATH /usr/local/opt/coreutils/libexec/gnubin $PATH
set -gx PATH ~/Utilities/bin $PATH
set -gx PATH ~/anaconda3/bin $PATH
set -gx PATH ~/.cargo/bin $PATH

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
end

# GLOBALS
set -gx TERM xterm-256color
set -gx EDITOR vim

# HOME
set -gx SCALA_HOME /usr/local/share/scala-2.11.0
set -gx JAVA_HOME /Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/Contents/Home

# Python
set -gx PYTHONPATH ~/Code/pelican-plugins $PYTHONPATH
set -gx PYTHONPATH ~/Code/qb $PYTHONPATH
set -gx PYTHONPATH ~/Utilities/spark-2.1.1-bin-hadoop2.7/python $PYTHONPATH
set -gx PYSPARK_PYTHON python3

# C/C++
set -gx CC clang
set -gx CXX "clang++"

# Quiz Bowl
set -gx QB_QUESTION_DB /Users/pedro/Documents/Code/qb/data/internal/naqt.db
set -gx QB_ROOT /Users/pedro/Documents/Code/qb
set -gx QB_SPARK_MASTER spark://terminus.local:7077
set -gx QB_AWS_S3_BUCKET entilzha-us-west-2
set -gx QB_AWS_S3_NAMESPACE atlanta-new-ingestion
set -gx QB_SECURITY_GROUPS sg-feab8f86
set -gx TF_VAR_key_pair pedro-key
set -gx TF_VAR_qb_aws_s3_bucket $QB_AWS_S3_BUCKET
set -gx TF_VAR_qb_aws_s3_namespace $QB_AWS_S3_NAMESPACE


# Secrets
. ~/.config/fish/secrets.fish

# SSH
ssh-add ~/pedro-key.pem ^ /dev/null
ssh-add ~/eos_id_rsa ^ /dev/null

# Aliases
alias s3jupyter="jupyter notebook --config ~/jupyter_s3_config.py"

# Configure man pages
set -gx LESS_TERMCAP_mb (printf '\e[01;31m') # enter blinking mode - red
set -gx LESS_TERMCAP_md (printf '\e[01;32m') # enter double-bright mode - bold, magenta
set -gx LESS_TERMCAP_me (printf '\e[0m') # turn off all appearance modes (mb, md, so, us)
set -gx LESS_TERMCAP_se (printf '\e[0m') # leave standout mode
set -gx LESS_TERMCAP_so (printf '\e[01;31m') # enter standout mode - yellow
set -gx LESS_TERMCAP_ue (printf '\e[0m') # leave underline mode
set -gx LESS_TERMCAP_us (printf '\e[04;36m') # enter underline mode - cyan


